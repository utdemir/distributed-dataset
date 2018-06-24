{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Provides a 'Backend' using AWS Lambda functions.
--
-- In order to do that, roughly:
--
--     * It creates a deployment archive containing the program binary and
--     a tiny wrapper written in Python and uploads it to the given S3 bucket.
--     * Creates a CloudFormation stack containing the Lambda function; and both
--     an SQS queue and an S3 Bucket and to gather the answers.
--     * It starts polling the SQS queue for any answers.
--     * When executing, it invokes the Lambda function using asynchronous
--     invocation mode. After the function finishes, it either puts the result
--     directly into the SQS queue if the result is small enough (< 200kb) or
--     uploads it into the S3 bucket and sends a pointer to the file via the
--     queue.
--     * A separate thread on the driver continously polls the queue for answers
--     and parses and returns it to the caller.
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
  , lboMaxConcurrentInvocations
  , lboMaxConcurrentExecutions
  , lboMaxConcurrentDownloads
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
import           Control.Concurrent.Throttled
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
--   'withLambdaBackend' opts $ \\backend -> do
--     handle <- 'fork' backend (static Dict) (static (return "Hello from Lambda!"))
--     await handle >>= putStrLn
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
    putStrLn $ "Stack created: " <> T.unpack (siId si)

    throttles <- (,,) <$> newThrottle _lboMaxConcurrentInvocations
                      <*> newThrottle _lboMaxConcurrentExecutions
                      <*> newThrottle _lboMaxConcurrentDownloads

    withInvoke env throttles si $ \invoke ->
      f $ Backend invoke

--------------------------------------------------------------------------------

-- |
-- Options required for creating a Lambda backend.
--
-- Use 'lambdaBackendOptions' smart constructor to create and lenses below for
-- setting optional fields.
data LambdaBackendOptions = LambdaBackendOptions
  { _lboBucket                   :: T.Text
  , _lboPrefix                   :: T.Text
  , _lboMemory                   :: Int
  , _lboMaxConcurrentInvocations :: Int
  , _lboMaxConcurrentExecutions  :: Int
  , _lboMaxConcurrentDownloads   :: Int
  }

lambdaBackendOptions :: T.Text -- ^ Name of the S3 bucket to store the deployment archive in.
                     -> LambdaBackendOptions
lambdaBackendOptions bucket =
  LambdaBackendOptions { _lboBucket = bucket
                       , _lboPrefix = "distributed-fork"
                       , _lboMemory = 128
                       , _lboMaxConcurrentInvocations = 64
                       , _lboMaxConcurrentExecutions  = 0
                       , _lboMaxConcurrentDownloads   = 16
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

-- |
-- Maximum number of concurrent "invoke" calls to AWS API to trigger executions.
--
-- Non-positive values disable the throttling.
--
-- Default: 64
lboMaxConcurrentInvocations :: Lens' LambdaBackendOptions Int
lboMaxConcurrentInvocations =
  lens _lboMaxConcurrentInvocations (\s t -> s { _lboMaxConcurrentInvocations = t })

-- |
-- Maximum number of concurrently executing Lambda functions.
--
-- Non-positive values disable the throttling.
--
-- Default: 0
lboMaxConcurrentExecutions :: Lens' LambdaBackendOptions Int
lboMaxConcurrentExecutions =
  lens _lboMaxConcurrentExecutions (\s t -> s { _lboMaxConcurrentExecutions = t })

-- |
-- If the size of the return value from your function is larger than 200 kilobytes,
-- we fetch the results via S3. This parameter sets the maximum number of concurrent
-- downloads from S3.
--
-- Non-positive values disable the throttling.
--
-- Default: 16
lboMaxConcurrentDownloads :: Lens' LambdaBackendOptions Int
lboMaxConcurrentDownloads =
  lens _lboMaxConcurrentDownloads (\s t -> s { _lboMaxConcurrentDownloads = t })
