{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}

module Control.Distributed.Fork.Internal where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Distributed.Closure
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Data.Binary
import Data.Binary.Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import GHC.Generics
import System.Environment
import System.Exit
import System.IO (stdin)

--------------------------------------------------------------------------------

-- |
-- We switch to executor mode only when  @argv[1] == argExecutorMode@.
argExecutorMode :: String
argExecutorMode = "DISTRIBUTED_FORK_EXECUTOR_MODE"

-- |
-- On distributed-fork, we run the same binary both in your machine (called
-- "driver") and in the remote environment (called "executor"). In order for the
-- program to act according to where it is, you should call this function as the
-- first thing in your @main@:
--
-- @
-- main = do
--   initDistributedFork
--   ...
-- @
initDistributedFork :: IO ()
initDistributedFork =
  getArgs >>= \case
    [x]
      | x == argExecutorMode -> vacuous runExecutor
    _ -> return ()

-- |
-- When on executor mode, we expect a serialised ExecutorClosure from `stdin`,
-- run it and exit.
runExecutor :: IO Void
runExecutor =
  BL.hGetContents stdin
    >>= unclosure
    . unZlibWrapper
    . decode @ExecutorClosure
    >> exitSuccess

-- |
-- An ExecutorClosure is a serialisable IO action.
type ExecutorClosure = ZlibWrapper (Closure (IO ()))

-- |
-- 'Backend' is responsible for running your functions in a remote environment.
--
-- See:
--
--   * 'Control.Distributed.Fork.Local.localProcessBackend'
--
--   * <http://hackage.haskell.org/package/distributed-dataset-aws distributed-dataset-aws>
newtype Backend
  = Backend
      { bExecute :: BS.ByteString -> BackendM BS.ByteString
      }
-- ^ Should run the current binary in the target environment, put the given
-- string as standard input and return the executables answer on the standard
-- output.
-- |
-- BackendM is essentially `IO`, but also has the ability to report the status of the
-- executor.

newtype BackendM a
  = BackendM (ReaderT (ExecutorPendingStatus -> IO ()) IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadUnliftIO
    )

instance Binary a => Binary (ExecutorFinalStatus a)

-- |
-- Given an IO action and a static proof that the result is 'Serializable', this
-- function runs the action using the Backend in a separate thread and returns a
-- 'TVar' holding the 'ExecutorStatus'.
runBackend ::
  Closure (Dict (Serializable i)) ->
  Closure (IO i) ->
  Backend ->
  IO (Handle i)
runBackend dict cls (Backend backend) =
  case unclosure dict of
    Dict -> do
      let BackendM m =
            backend $ toBackendStdin dict cls
      t <- atomically (newTVar $ ExecutorPending (ExecutorWaiting Nothing))
      _ <-
        forkIO $ do
          answer <- try $ runReaderT m (atomically . writeTVar t . ExecutorPending)
          let r =
                either
                  ( \(err :: SomeException) ->
                      ExecutorFailed $
                        "Backend threw an exception: "
                          <> T.pack (show err)
                  )
                  fromBackendStdout
                  answer
          atomically $ writeTVar t (ExecutorFinished r)
          return ()
      return $ Handle t

toBackendStdin :: Closure (Dict (Serializable a)) -> Closure (IO a) -> BS.ByteString
toBackendStdin dict cls =
  case unclosure dict of
    Dict -> BL.toStrict . encode . ZlibWrapper $ static run `cap` dict `cap` cls
  where
    run :: forall a. Dict (Serializable a) -> IO a -> IO ()
    run Dict a =
      (a >>= BL.putStr . encode . ExecutorSucceeded)
        `catch` ( \(ex :: SomeException) ->
                    BL.putStr . encode . ExecutorFailed @a $
                      "Exception from executor: "
                        <> T.pack (show ex)
                )

fromBackendStdout :: Binary a => BS.ByteString -> ExecutorFinalStatus a
fromBackendStdout bs =
  case decodeOrFail (BL.fromStrict bs) of
    Left (_, _, err) -> ExecutorFailed $ "Error decoding answer: " <> T.pack err
    Right (_, _, a) -> a

data ExecutorStatus a
  = ExecutorPending ExecutorPendingStatus
  | ExecutorFinished (ExecutorFinalStatus a)
  deriving (Eq, Functor)

data ExecutorPendingStatus
  = ExecutorWaiting (Maybe Text)
  | ExecutorSubmitted (Maybe Text)
  | ExecutorStarted (Maybe Text)
  deriving (Eq)

data ExecutorFinalStatus a
  = ExecutorFailed Text
  | ExecutorSucceeded a
  deriving (Eq, Generic, Functor)

-- |
-- Result of a 'fork' is an Handle where you can 'await' a result.
newtype Handle a = Handle (TVar (ExecutorStatus a))

-- |
-- Get the current status of given 'Handle'.
pollHandle :: Handle a -> STM (ExecutorStatus a)
pollHandle (Handle t) = readTVar t

tryAwait :: Handle a -> IO (Either Text a)
tryAwait h = do
  r <-
    liftIO . atomically $
      pollHandle h >>= \case
        ExecutorPending _ -> retry
        ExecutorFinished a -> return a
  return $ case r of
    ExecutorFailed err -> Left err
    ExecutorSucceeded a -> Right a
