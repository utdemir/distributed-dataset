{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Network.Serverless.Execute.Lambda
  ( withLambdaBackend
  -- * Options
  , LambdaBackendOptions
  , lambdaBackendOptions
  , lboPrefix
  , lboMemory
  ) where

--------------------------------------------------------------------------------
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
      bool (liftIO (putStrLn $ "Uploading the deployment archive. (" +| fixedF 2 size |+ " MB)")
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

data LambdaBackendOptions = LambdaBackendOptions
  { _lboBucket      :: T.Text
  , _lboPrefix      :: T.Text
  , _lboMemory      :: Int
  }

lambdaBackendOptions :: T.Text -- Name of the S3 bucket to store the deployment archive in.
                     -> LambdaBackendOptions
lambdaBackendOptions bucket =
  LambdaBackendOptions { _lboBucket = bucket
                       , _lboPrefix = "serverless-execute"
                       , _lboMemory = 128
                       }

-- | Desired memory for the Lambda functions.
--   Default: 128
lboMemory :: Lens' LambdaBackendOptions Int
lboMemory = lens _lboMemory (\s t -> s { _lboMemory = t })

-- | Prefix of the name of deployment archive and the CloudFormation stack.
--   Default: "serverless-execute"
lboPrefix :: Lens' LambdaBackendOptions T.Text
lboPrefix = lens _lboPrefix (\s t -> s { _lboPrefix = t })

