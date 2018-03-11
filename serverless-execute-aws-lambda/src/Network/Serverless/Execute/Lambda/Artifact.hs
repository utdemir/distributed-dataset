{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.Serverless.Execute.Lambda.Artifact where

--------------------------------------------------------------------------------
import System.Environment
import Data.FileEmbed
import Data.Monoid
import Control.Monad
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Codec.Archive.Zip
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BL
import System.Process.Typed
--------------------------------------------------------------------------------

newtype HandlerPy =
  HandlerPy BL.ByteString

newtype MainHs =
  MainHs BL.ByteString

newtype DeploymentZip =
  DeploymentZip BL.ByteString

mkArtifact :: IO BL.ByteString
mkArtifact = do
  mainHs <- getExecutablePath >>= mkMainHs >>= \case
    Left err -> error $ T.unpack err
    Right t -> return t
  case mkDeploymentZip handlerPy mainHs of
    DeploymentZip bl -> return bl


mkDeploymentZip :: HandlerPy -> MainHs -> DeploymentZip
mkDeploymentZip (HandlerPy handler) (MainHs executable) =
  DeploymentZip . fromArchive $ archive
  where
    archive =
      addEntryToArchive (toEntry "hander.py" 0 handler) $
      addEntryToArchive
        (toEntry "hs-main" 0 executable)
          { eExternalFileAttributes = 0b10000 -- rwx
          } $
      emptyArchive

handlerPy :: HandlerPy
handlerPy = HandlerPy $ BL.fromStrict $(embedFile "static/handler.py")

mkMainHs :: FilePath -> IO (Either T.Text MainHs)
mkMainHs fp =
  runExceptT $ existence >> file >> (MainHs <$> liftIO (BL.readFile fp))
  where
    existence =
      liftIO (doesFileExist fp) >>= \case
        False -> throwE $ "File does not exist: " <> (T.pack fp)
        True -> return ()
    file = do
      ret <-
        liftIO $
        T.splitOn ", " . T.decodeUtf8 . BL.toStrict <$>
        readProcessStdout_ (proc "file" ["--brief", fp])
      let preds :: [(T.Text, T.Text)]
          preds =
            [ ( "ELF 64-bit LSB executable"
              , "file is not an 64 bit ELF executable")
            , ("statically linked", "file is not statically linked")
            ]
      forM_ preds $ \(str, ex) ->
        if str `elem` ret
          then return ()
          else throwE ex
      return ()
