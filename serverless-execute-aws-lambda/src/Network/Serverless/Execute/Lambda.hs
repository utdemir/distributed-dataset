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
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import Control.Monad.Catch
import Data.ByteString.Base64 as B64
import Control.Monad.IO.Class
import Data.Time
import Network.Serverless.Execute.Backend
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Lambda.Internal.Stack
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
  undefined
--  artifact <- mkArtifact
--  let cksum = artifactChecksum artifact
--      size = artifactSize artifact
--      s3loc =
--        S3Loc (BucketName _lboBucket) (_lboPrefix <> "-" <> cksum <> ".zip")
--  time <-
--    T.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
--  putStrLn $
--    "Uploading the artifact. (" <>
--    show (round ((fromIntegral size :: Double) / (1000 * 1000)) :: Int) <>
--    " MB)"
--  runResourceT . runAWS env $
--    awsUploadObject s3loc (artifactToByteString artifact)
--  putStrLn "Artifact uploaded."
--
--  putStrLn "Creating stack."
--  withStack
--    env
--    (StackName (_lboStackPrefix <> "-" <> time <> "-" <> cksum))
--    s3loc
--    (\i -> putStrLn "Stack created." >> f (toBackend env i))
--  where
--    toBackend env lambda =
--      Backend $ \bs -> do
--        let params =
--              HM.fromList
--                [("d", String . T.decodeUtf8 . B64.encode . BL.toStrict $ bs)]
--
--        r <- liftIO . runResourceT . runAWS env $ awsInvoke lambda params
--        case "d" `HM.lookup` r of
--          Just (String x) ->
--            case B64.decode $ T.encodeUtf8 x of
--              Left err ->
--                throwM $ AWSError ("Error decoding answer: " <> T.pack err) x
--              Right ret -> return $ BL.fromStrict ret
--          _ -> throwM $ AWSError "Error decoding answer." (T.pack $ show r)
