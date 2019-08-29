{-|
You only need this module if you want to create a new backend for distributed-dataset.

See 'Control.Distributed.Fork.Local.localProcessBackend' for a minimal example.
-}
module Control.Distributed.Fork.Backend
  ( -- * Writing a 'Backend'.
    Backend (..),
    BackendM,
    argExecutorMode,
    -- * Reporting status
    ExecutorFinalStatus (..),
    ExecutorStatus (..),
    ExecutorPendingStatus (..),
    waiting,
    waitingDesc,
    submitted,
    submittedDesc,
    started,
    startedDesc,
    -- * Utils
    throttledBackend,
    -- * Re-exports
    liftIO,
    getExecutablePath
    )
where

--------------------------------------------------------------------------------
import Control.Concurrent.Throttled
--------------------------------------------------------------------------------
import Control.Distributed.Fork.Internal
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Text (Text)
import System.Environment

--------------------------------------------------------------------------------
pendingStatus :: ExecutorPendingStatus -> BackendM ()
pendingStatus s = BackendM ask >>= liftIO . ($ s)

waiting :: BackendM ()
waiting = pendingStatus $ ExecutorWaiting Nothing

waitingDesc :: Text -> BackendM ()
waitingDesc = pendingStatus . ExecutorWaiting . Just

submitted :: BackendM ()
submitted = pendingStatus $ ExecutorSubmitted Nothing

submittedDesc :: Text -> BackendM ()
submittedDesc = pendingStatus . ExecutorSubmitted . Just

started :: BackendM ()
started = pendingStatus $ ExecutorStarted Nothing

startedDesc :: Text -> BackendM ()
startedDesc = pendingStatus . ExecutorStarted . Just

throttledBackend :: MonadIO m => Int -> Backend -> m Backend
throttledBackend limit (Backend b) = do
  t <- newThrottle limit
  return $ Backend (throttled t . b)
