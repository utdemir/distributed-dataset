module Network.Serverless.Execute.LocalProcessBackend where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy               as BL
import           System.Process.Typed
--------------------------------------------------------------------------------
import           Network.Serverless.Execute.Backend
--------------------------------------------------------------------------------

-- |
-- A 'Backend' which uses local processes as executors. Useful for testing.
localProcessBackend :: Backend
localProcessBackend = Backend $ \stdin' -> do
  executable <- liftIO getExecutablePath
  let conf =
        setStdin (byteStringInput $ BL.fromStrict stdin') $
        proc executable [const_SERVERLESS_EXECUTOR_MODE]
  liftIO . fmap BL.toStrict $ readProcessStdout_ conf
