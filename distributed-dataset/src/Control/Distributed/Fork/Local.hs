module Control.Distributed.Fork.Local where

import Control.Distributed.Fork.Backend
import qualified Data.ByteString.Lazy as BL
import System.Process.Typed

-- |
-- A 'Backend' which uses local processes as executors. Useful for testing.
localProcessBackend :: Backend
localProcessBackend =
  Backend $ \stdin' -> do
    executable <- liftIO getExecutablePath
    let conf =
          setStdin (byteStringInput $ BL.fromStrict stdin') $
            proc executable [argExecutorMode]
    liftIO . fmap BL.toStrict $ readProcessStdout_ conf
