{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Provides a 'Backend' using AWS Lambda functions.
--
-- In order to do that, roughly:
--
--     * It creates a deployment archive containing the program binary and
--     a tiny wrapper written in Python and uploads it to the given S3 bucket.
--     * Creates a CloudFormation stack containing the Lambda function and
--     an SQS queue to gather the answers.
--     * It starts polling the SQS queue for any answers.
--     * When executing, it invokes the Lambda function using asynchronous
--     invocation mode and blocks until a message appears in the queue.
--     * On exit, it deletes the CloudFormation stack.
--
-- Some warts:
--
--     * On AWS Lambda, more memory you assign to a function, more CPU you
--     get. So it might make your function run faster if you overallocate
--     memory.
--     * This library only sends a single short-lived HTTPS request to invoke
--     a Lambda function and does not keep a socket open when waiting for
--     an answer. However if you spawn thousands of functions at once, you
--     might run out of file descriptors in your host machine. To overcome
--     this, put a small delay (@(threadDelay 10000)@ was enough in my case)
--     between your calls when you're invoking more than a few hundred
--     functions.
--     * When invoked asynchronously, AWS Lambda retries the invocation
--     2 more times waiting a minute between every retry. This means when
--     something fails, it will take at least a few minutes to until you
--     get an exception.

module Network.Serverless.Execute.Lambda
  (
  -- * Usage
    withLambdaBackend
  -- * Options
  , LambdaBackendOptions
  , lambdaBackendOptions
  , lboPrefix
  , lboMemory
  ) where

import           Data.Bool                                          (bool)
import           Data.Monoid                                        ((<>))
import qualified Data.Text                                          as T
import           Data.Time.Clock                                    (getCurrentTime)
import           Data.Time.Format                                   (defaultTimeLocale,
                                                                     formatTime)
import           Fmt                                                (fixedF,
                                                                     (+|), (|+))
import           Control.Lens                                       ((^.), Lens', lens)
import           Network.AWS                                        (Credentials (Discover),
                                                                     envRegion,
                                                                     newEnv,
                                                                     runAWS,
                                                                     runResourceT)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute.Backend
import           Network.Serverless.Execute.Lambda.Internal.Archive
import           Network.Serverless.Execute.Lambda.Internal.Invoke
import           Network.Serverless.Execute.Lambda.Internal.Stack
import           Network.Serverless.Execute.Lambda.Internal.Types
--------------------------------------------------------------------------------

-- |
-- Provides a 'Backend' using AWS Lambda functions.
--
-- Created AWS resources except the uploaded deployment package will be cleaned
-- up when exiting.
--
-- Example usage:
--
-- @
-- {-\# LANGUAGE StaticPointers #-}
--
-- import Control.Lens
-- import Network.Serverless.Execute
-- import Network.Serverless.Execute.Lambda
--
-- opts :: LambdaBackendOptions
-- opts = lambdaBackendOptions "my-s3-bucket"
--          & lboMemory .~ 1024
--
-- main :: IO ()
-- main = do
--   'initServerless'
--   'withLambdaBackend' opts $ \backend -> do
--     ret <- 'execute' 'backend' (static 'Dict') (static (return "Hello from Lambda!"))
--     putStrLn ret
-- @

withLambdaBackend :: LambdaBackendOptions -> (Backend -> IO a) -> IO a
withLambdaBackend LambdaBackendOptions {..} f = do
  env <- newEnv Discover
  putStrLn $ "Detected region: " +| show (env ^. envRegion) |+ "."

  archive <- mkArchive
  let cksum = archiveChecksum archive
      size  = fromIntegral (archiveSize archive) / (1000 * 1000) :: Double
      s3loc =
        S3Loc (BucketName _lboBucket) (_lboPrefix <> "-" <> cksum <> ".zip")

  putStrLn $
    "Checking if deployment archive exists."
  runResourceT . runAWS env $
    awsObjectExists s3loc >>=
      bool
        (liftIO (putStrLn $ "Uploading the deployment archive. (" +| fixedF 2 size |+ " MB)")
          >> awsUploadObject  s3loc (archiveToByteString archive))
        (liftIO $ putStrLn "Found archive, skipping upload.")


  time <-
    T.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
  let stackOptions =
        StackOptions { soName = StackName (_lboPrefix <> "-" <> time <> "-" <> cksum)
                     , soLambdaMemory = _lboMemory
                     , soLambdaCode = s3loc
                     }

  putStrLn "Creating stack."
  withStack stackOptions env $ \si -> do
    putStrLn "Stack created."
    putStrLn $ "Stack Id: " +| siId si |+ ""
    putStrLn $ "Func: " +| siFunc si |+ ""
    putStrLn $ "Answer Queue: " +| siAnswerQueue si |+ ""
    putStrLn $ "Dead Letter Queue: " +| siDeadLetterQueue si |+ ""
    withInvoke env si $ \invoke ->
      f $ Backend invoke

--------------------------------------------------------------------------------

-- |
-- Options required for creating a Lambda backend.
--
-- Use 'lambdaBackendOptions' smart constructor to create and lenses below for
-- setting optional fields.
data LambdaBackendOptions = LambdaBackendOptions
  { _lboBucket      :: T.Text
  , _lboPrefix      :: T.Text
  , _lboMemory      :: Int
  }

lambdaBackendOptions :: T.Text -- ^ Name of the S3 bucket to store the deployment archive in.
                     -> LambdaBackendOptions
lambdaBackendOptions bucket =
  LambdaBackendOptions { _lboBucket = bucket
                       , _lboPrefix = "serverless-execute"
                       , _lboMemory = 128
                       }

-- |
-- Desired memory for the Lambda functions.
--
-- See <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-function.html#cfn-lambda-function-memorysize CloudFormation's AWS::Lambda::Function::MemorySize page> for allowed values.
--
--   Default: 128
lboMemory :: Lens' LambdaBackendOptions Int
lboMemory = lens _lboMemory (\s t -> s { _lboMemory = t })

-- |
-- Prefix to the deployment archive and the CloudFormation stack.
--
-- Default: "serverless-execute"
lboPrefix :: Lens' LambdaBackendOptions T.Text
lboPrefix = lens _lboPrefix (\s t -> s { _lboPrefix = t })

