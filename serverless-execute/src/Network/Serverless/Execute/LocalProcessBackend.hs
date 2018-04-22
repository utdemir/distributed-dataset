module Network.Serverless.Execute.LocalProcessBackend where

--------------------------------------------------------------------------------
import System.Process.Typed
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Backend
--------------------------------------------------------------------------------

-- |
-- A 'Backend' which uses local processes as executors.
localProcessBackend :: Backend
localProcessBackend = Backend $ \stdin' -> do
  executable <- liftIO getExecutablePath
  let conf =
        setStdin (byteStringInput stdin') $
        proc executable [const_SERVERLESS_EXECUTOR_MODE]
  liftIO $ readProcessStdout_ conf
