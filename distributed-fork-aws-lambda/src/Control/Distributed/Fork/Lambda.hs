{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
--     * The same binary should run on AWS Lambda. In practice, this means:
--
--         * You have to build and use this library on a Linux machine.
--
--         * You have to statically link everything. You can use GHC's
--         '@-static -optl-static -optl-pthread -fPIC@' parameters for that.
--         See <https://github.com/utdemir/serverless-batch/blob/master/examples/examples.cabal examples/examples.cabal#L16>
--         for an example.
--
--     * On AWS Lambda, more memory you assign to a function, more CPU you
--     get. So it might make your function run faster if you overallocate
--     memory.
--     * When invoked asynchronously, AWS Lambda retries the invocation
--     2 more times waiting a minute between every retry. This means when
--     something fails, it will take at least a few minutes to until you
--     get an exception.

module Control.Distributed.Fork.Lambda
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
import           Control.Lens                                       ((^.), Lens', lens)
import           Network.AWS                                        (Credentials (Discover),
                                                                     envRegion,
                                                                     newEnv,
                                                                     runAWS,
                                                                     runResourceT)
--------------------------------------------------------------------------------
import           Control.Distributed.Fork.Backend
import           Control.Distributed.Fork.Lambda.Internal.Archive
import           Control.Distributed.Fork.Lambda.Internal.Invoke
import           Control.Distributed.Fork.Lambda.Internal.Stack
import           Control.Distributed.Fork.Lambda.Internal.Types
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
-- import Control.Distributed.Fork
-- import Control.Distributed.Fork.Lambda
--
-- opts :: LambdaBackendOptions
-- opts = lambdaBackendOptions "my-s3-bucket"
--          & lboMemory .~ 1024
--
-- main :: IO ()
-- main = do
--   'initDistributedFork'
--   'withLambdaBackend' opts $ \backend -> do
--     ret <- 'execute' 'backend' (static 'Dict') (static (return "Hello from Lambda!"))
--     putStrLn ret
-- @

withLambdaBackend :: LambdaBackendOptions -> (Backend -> IO a) -> IO a
withLambdaBackend LambdaBackendOptions {..} f = do
  env <- newEnv Discover
  putStrLn $ "Detected region: " <> show (env ^. envRegion) <> "."

  archive <- mkArchive
  let cksum = archiveChecksum archive
      size  = fromIntegral (archiveSize archive) / (1000 * 1000) :: Double
      s3loc =
        S3Loc (BucketName _lboBucket) (_lboPrefix <> "-" <> cksum <> ".zip")

  putStrLn "Checking if deployment archive exists."
  runResourceT . runAWS env $
    awsObjectExists s3loc >>=
      bool
        (liftIO (putStrLn $ "Uploading the deployment archive. (" <> show size <> " MB)")
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
    putStrLn $ "Artifact: " <> show s3loc
    putStrLn $ "Stack Id: " <> T.unpack (siId si)
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
                       , _lboPrefix = "distributed-fork"
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
-- Default: "distributed-fork"
lboPrefix :: Lens' LambdaBackendOptions T.Text
lboPrefix = lens _lboPrefix (\s t -> s { _lboPrefix = t })

