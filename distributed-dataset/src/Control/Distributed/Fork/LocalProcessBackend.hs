module Control.Distributed.Fork.LocalProcessBackend where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy             as BL
import           System.Process.Typed
--------------------------------------------------------------------------------
import           Control.Distributed.Fork.Backend
--------------------------------------------------------------------------------

-- |
-- A 'Backend' which uses local processes as executors. Useful for testing.
localProcessBackend :: Backend
localProcessBackend = Backend $ \stdin' -> do
  executable <- liftIO getExecutablePath
  let conf =
        setStdin (byteStringInput $ BL.fromStrict stdin') $
        proc executable [argExecutorMode]
  liftIO . fmap BL.toStrict $ readProcessStdout_ conf
