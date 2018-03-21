{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Serverless.Execute.Internal where

--------------------------------------------------------------------------------
import System.IO
import System.Exit
import qualified Data.ByteString.Lazy as BL
import Control.Distributed.Closure
import Data.Text (Text)
import Data.Binary
import qualified Data.Text as T
import Control.Monad.Catch
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Void
import System.Environment
import Control.Concurrent
--------------------------------------------------------------------------------

-- |
-- On serverless-batch, we run the same binary both in the users machine (called
-- "driver") and the remote environment (called "executor"). The executable starts
-- in driver mode by default, and runs in the executor mode only if
-- @argv[1] == const_SERVERLESS_EXECUTOR_MODE@.
const_SERVERLESS_EXECUTOR_MODE :: String
const_SERVERLESS_EXECUTOR_MODE = "SERVERLESS_EXECUTOR_MODE"

-- |
-- On serverless-batch, we run the same binary both in the users machine (called
-- "driver") and in the remote environment (called "executor"). In order for the
-- program to act according to where it is, you should call this function as the
-- first thing in your @main@:
--
-- @
-- main = do
--   serverlessGuard
--   ...
-- @
serverlessGuard :: IO ()
serverlessGuard = do
  getArgs >>= \case
    [x]
      | x == const_SERVERLESS_EXECUTOR_MODE -> vacuous runExecutor
    _ -> return ()

-- |
-- When on executor mode, we expect a serialised ExecutorClosure from `stdin`,
-- run it and exit.
runExecutor :: IO Void
runExecutor = do
  BL.hGetContents stdin
    >>= unclosure . decode @ExecutorClosure
    >> exitWith ExitSuccess

-- |
-- An ExecutorClosure is a serialisable IO action.
type ExecutorClosure = Closure (IO ())

-- |
-- 'Backend' is responsible for running your functions in a remote environment.
--
-- See:
--
--   * 'Network.Serverless.Execute.LocalProcessBackend.localProcessBackend'
--
--   * <http://hackage.haskell.org/package/serverless-execute-aws-lambda serverless-execute-aws-lambda>
data Backend = Backend
  { bExecute :: BL.ByteString -> BackendM BL.ByteString
  -- ^ Should run the current binary in the target environment, put the given
  -- string as standard input and return the executables answer on the standard
  -- output.
  }

-- |
-- BackendM is a `MonadIO`, but also has the ability to report the status of the
-- executor.
newtype BackendM a =
  BackendM (ReaderT (ExecutorPendingStatus -> IO ()) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

data ExecutorStatus a
  = ExecutorPending ExecutorPendingStatus
  | ExecutorFinished (ExecutorFinalStatus a)

data ExecutorPendingStatus
  = ExecutorWaiting (Maybe Text)
  | ExecutorSubmitted (Maybe Text)
  | ExecutorStarted (Maybe Text)

data ExecutorFinalStatus a
  = ExecutorFailed Text
  | ExecutorSucceeded a

-- |
-- Given an IO action and a static proof that the result is 'Serializable', this
-- function runs the action using the Backend in a separate thread and returns a
-- 'TVar' holding the 'ExecutorStatus'.
runBackend ::
     Closure (Dict (Serializable i))
  -> Closure (IO i)
  -> Backend
  -> IO (TVar (ExecutorStatus i))
runBackend dict cls (Backend backend) =
  case unclosure dict of
    Dict -> do
      let BackendM m =
            backend $ encode @ExecutorClosure (toExecutorClosure dict cls)
      t <- atomically (newTVar $ ExecutorPending (ExecutorWaiting Nothing))
      _ <-
        forkIO $ do
          r <-
            either
              (\(err :: SomeException) ->
                 ExecutorFailed $
                 "Backend threw an exception: " <> T.pack (show err))
              parseAnswer <$>
            try (runReaderT m (atomically . writeTVar t . ExecutorPending))
          atomically $ writeTVar t (ExecutorFinished r)
          return ()
      return t
  
toExecutorClosure :: Closure (Dict (Serializable a)) -> Closure (IO a) -> Closure (IO ())
toExecutorClosure dict cls =
  case unclosure dict of
    Dict -> static (\Dict i -> i >>= BL.putStr . encode) `cap` dict `cap` cls

parseAnswer :: Binary a => BL.ByteString -> ExecutorFinalStatus a
parseAnswer bs =
  case decodeOrFail bs of
    Left (_, _, err) -> ExecutorFailed $ "Error decoding answer: " <> T.pack err
    Right (_, _, a) -> ExecutorSucceeded a

