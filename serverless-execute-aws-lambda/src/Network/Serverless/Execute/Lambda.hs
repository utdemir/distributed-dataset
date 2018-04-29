{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Serverless.Execute.Lambda
  ( withLambdaBackend
  , LambdaBackendOptions(..)
  ) where

--------------------------------------------------------------------------------
import Network.AWS
import qualified Data.Text as T
import Data.Monoid
import Network.Serverless.Execute.Backend
import Data.Time.Clock
import Data.Bool
import Data.Time.Format
import Lens.Micro
import Fmt
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Lambda.Internal.Stack
import Network.Serverless.Execute.Lambda.Internal.Invoke
import Network.Serverless.Execute.Lambda.Internal.Archive
import Network.Serverless.Execute.Lambda.Internal.Types
--------------------------------------------------------------------------------

data LambdaBackendOptions = LambdaBackendOptions
  { _lboBucket :: T.Text
  , _lboPrefix :: T.Text
  , _lboStackPrefix :: T.Text
  }

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
  let stackName = StackName (_lboStackPrefix <> "-" <> time <> "-" <> cksum)

  putStrLn "Creating stack."
  withStack env stackName s3loc $ \si -> do
    putStrLn "Stack created."
    putStrLn $ "Stack Id: " +| siId si |+ ""
    putStrLn $ "Func: " +| siFunc si |+ ""
    putStrLn $ "Answer Queue: " +| siAnswerQueue si |+ ""
    putStrLn $ "Dead Letter Queue: " +| siDeadLetterQueue si |+ ""
    withInvoke env si $ \invoke ->
      f $ Backend invoke
