module Network.Serverless.Execute.Backend
  ( Backend (..)
  , BackendM
  , const_SERVERLESS_EXECUTOR_MODE
  , ExecutorFinalStatus (..)
  , ExecutorStatus(..)
  , ExecutorPendingStatus (..)
  , waiting, waitingDesc
  , submitted, submittedDesc
  , started, startedDesc

  -- * Re-exports
  , liftIO
  , getExecutablePath
  ) where

--------------------------------------------------------------------------------
import System.Environment
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Internal
--------------------------------------------------------------------------------

pendingStatus :: ExecutorPendingStatus -> BackendM ()
pendingStatus s = BackendM ask >>= liftIO . ($ s)

-- | When writing a Backend, you can use these to report the status

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
