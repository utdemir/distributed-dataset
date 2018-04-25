{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Serverless.Execute.Lambda.Artifact
  ( mkArtifact
  , artifactChecksum
  , artifactSize
  , Artifact (..)
  ) where

--------------------------------------------------------------------------------
import Data.Digest.Pure.SHA
import System.Environment
import Data.String.Interpolate
import Data.Monoid
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BL
import Network.Serverless.Execute.Backend
--------------------------------------------------------------------------------

newtype HandlerPy =
  HandlerPy BL.ByteString

newtype MainHs =
  MainHs BL.ByteString

newtype Artifact =
  Artifact { artifactToByteString :: BL.ByteString }

mkArtifact :: IO Artifact
mkArtifact = do
  mainHs <-
    getExecutablePath >>= mkMainHs >>= \case
      Left err -> error $ T.unpack err
      Right t -> return t
  return $ archiveArtifact handlerPy mainHs

artifactChecksum :: Artifact -> T.Text
artifactChecksum = T.pack . showDigest . sha1 . artifactToByteString

artifactSize :: Artifact -> Integer
artifactSize = fromIntegral . BL.length . artifactToByteString

archiveArtifact :: HandlerPy -> MainHs -> Artifact
archiveArtifact (HandlerPy handler) (MainHs executable) =
  Artifact . fromArchive $ archive
  where
    archive =
      addEntryToArchive (toEntry "handler.py" 0 handler) $
      addEntryToArchive
        (toEntry "hs-main" 0 executable)
          { eExternalFileAttributes = 0b10000 -- rwx
          } $
      emptyArchive

handlerPy :: HandlerPy
handlerPy = HandlerPy $ BL.fromStrict $ T.encodeUtf8 $ T.pack [i|
import os
import subprocess
from base64 import *
import boto3

queue_url = os.environ["ANSWER_QUEUE_URL"]
client = boto3.client('sqs')

def handle(event, context):
    popen = subprocess.Popen(
       ["./hs-main", "#{const_SERVERLESS_EXECUTOR_MODE}"],
       stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (out, _) = popen.communicate(b64decode(event["d"]))
    client.send_message(
      QueueUrl=queue_url,
      MessageBody=b64encode(out)
    
|]

mkMainHs :: FilePath -> IO (Either T.Text MainHs)
mkMainHs fp = runExceptT $ existence >> (MainHs <$> liftIO (BL.readFile fp))
  where
    existence =
      liftIO (doesFileExist fp) >>= \case
        False -> throwE $ "File does not exist: " <> T.pack fp
        True -> return ()
--    file = do
--      ret <-
--        liftIO $
--        T.splitOn ", " . T.decodeUtf8 . BL.toStrict <$>
--        readProcessStdout_ (proc "file" ["--brief", fp])
--      let preds :: [(T.Text, T.Text)]
--          preds =
--            [ ( "ELF 64-bit LSB executable"
--              , "file is not an 64 bit ELF executable")
--            , ( "statically linked"
--              , "file is not statically linked"
--              )
--            ]
--      forM_ preds $ \(str, ex) ->
--        if str `elem` ret
--        then return ()
--        else throwE ex
--      return ()

-- archiveArtifact :: HandlerPy -> MainHs -> IO Artifact
-- archiveArtifact (HandlerPy handler) (MainHs executable) = do
--   now <- utcToLocalTime utc <$> getCurrentTime
--   let handlerEntry =
--         (ZipEntry "handler.py" now Nothing 0, ZipDataByteString handler)
--       executableEntry =
--         (ZipEntry "hs-main" now Nothing 0b10000, ZipDataByteString executable)
--   contents <-
--     runConduitRes $
--       (yield handlerEntry >> yield executableEntry)
--         .| void (zipStream defaultZipOptions { zipOptCompressLevel = 9 })
--         .| sinkLazy
--   return $ Artifact contents
