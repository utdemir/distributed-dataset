{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}

module Network.Serverless.Execute.Internal
  ( const_SERVERLESS_EXECUTOR_MODE
  , initServerless
  , Backend (..)
  , BackendM (..)
  , ExecutorStatus (..)
  , ExecutorPendingStatus (..)
  , ExecutorFinalStatus (..)
  , runBackend
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Distributed.Closure
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Binary
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Void
import           System.Environment
import           System.Exit
import           System.IO
import GHC.Generics
--------------------------------------------------------------------------------

-- |
-- We switch to executor mode only when  @argv[1] == const_SERVERLESS_EXECUTOR_MODE@.
const_SERVERLESS_EXECUTOR_MODE :: String
const_SERVERLESS_EXECUTOR_MODE = "SERVERLESS_EXECUTOR_MODE"

-- |
-- On serverless-execute, we run the same binary both in your machine (called
-- "driver") and in the remote environment (called "executor"). In order for the
-- program to act according to where it is, you should call this function as the
-- first thing in your @main@:
--
-- @
-- main = do
--   initServerless
--   ...
-- @
initServerless :: IO ()
initServerless = do
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
  { bExecute :: BS.ByteString -> BackendM BS.ByteString
  -- ^ Should run the current binary in the target environment, put the given
  -- string as standard input and return the executables answer on the standard
  -- output.
  }

-- |
-- BackendM is essentially `IO`, but also has the ability to report the status of the
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
  deriving (Generic)

instance Binary a => Binary (ExecutorFinalStatus a)

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
            backend $ BL.toStrict $ encode @ExecutorClosure (toExecutorClosure dict cls)
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
    Dict -> static run `cap` dict `cap` cls
  where
    run :: forall a. Dict (Serializable a) -> IO a -> IO ()
    run Dict a =
      (a >>= BL.putStr . encode . ExecutorSucceeded)
        `catch` (\(ex :: SomeException) ->
          BL.putStr . encode . ExecutorFailed @a $
            "Exception from executor: " <> T.pack (show ex))

parseAnswer :: Binary a => BS.ByteString -> ExecutorFinalStatus a
parseAnswer bs =
  case decodeOrFail (BL.fromStrict bs) of
    Left (_, _, err) -> ExecutorFailed $ "Error decoding answer: " <> T.pack err
    Right (_, _, a) -> a
