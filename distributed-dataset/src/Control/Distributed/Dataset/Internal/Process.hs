{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Control.Distributed.Dataset.Internal.Process where

--------------------------------------------------------------------------------
import Conduit
import Data.Binary
import qualified Data.ByteString as BS
import Data.IORef
import GHC.Generics
import System.Clock

--------------------------------------------------------------------------------
data ExecutorResponse a
  = ExecutorResponse
      { erStats :: ExecutorStats,
        erResponse :: a
      }
  deriving (Show, Generic, Binary)

data ExecutorStats
  = ExecutorStats
      { esDownloadBytes :: Integer,
        esUploadBytes :: Integer,
        esInputItems :: Integer,
        esOutputItems :: Integer,
        esElapsedMillis :: Integer
      }
  deriving (Show, Generic, Binary)

instance Semigroup ExecutorStats where
  ExecutorStats a b c d e <> ExecutorStats a' b' c' d' e' =
    ExecutorStats (a + a') (b + b') (c + c') (d + d') (e + e')

instance Monoid ExecutorStats where
  mempty = ExecutorStats 0 0 0 0 0

data ExecutorStatsHooks
  = ExecutorStatsHooks
      { eshDownload :: forall m. MonadIO m => ConduitT BS.ByteString BS.ByteString m (),
        eshUpload :: forall m. MonadIO m => ConduitT BS.ByteString BS.ByteString m (),
        eshInput :: forall a m. MonadIO m => ConduitT a a m (),
        eshOutput :: forall a m. MonadIO m => ConduitT a a m ()
      }

withExecutorStats :: (ExecutorStatsHooks -> IO a) -> IO (ExecutorResponse a)
withExecutorStats act = do
  downloadRef <- newIORef 0
  uploadRef <- newIORef 0
  inputRef <- newIORef 0
  outputRef <- newIORef 0
  let countBs ref =
        iterMC $ \bs ->
          liftIO $ modifyIORef ref (+ fromIntegral (BS.length bs))
      countIt ref =
        iterMC $ \_ ->
          liftIO $ modifyIORef ref (+ 1)
      hooks =
        ExecutorStatsHooks
          { eshDownload = countBs downloadRef,
            eshUpload = countBs uploadRef,
            eshInput = countIt inputRef,
            eshOutput = countIt outputRef
          }
  start <- getTime Monotonic
  ret <- act hooks
  end <- getTime Monotonic
  let elapsed = toNanoSecs (end - start) `div` 1000000
  downloadBytes <- readIORef downloadRef
  uploadBytes <- readIORef uploadRef
  inputCount <- readIORef inputRef
  outputCount <- readIORef outputRef
  let stats =
        ExecutorStats
          { esDownloadBytes = downloadBytes,
            esUploadBytes = uploadBytes,
            esInputItems = inputCount,
            esOutputItems = outputCount,
            esElapsedMillis = elapsed
          }
  return $ ExecutorResponse stats ret
