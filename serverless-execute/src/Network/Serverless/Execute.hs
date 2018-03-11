{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.Serverless.Execute
  ( execute
  , spawn
  , ExecutorStatus (..)
  , ExecutorPendingStatus (..)
  , ExecutorFinalStatus (..)
  , serverlessGuard

  -- Writing backends
  , Backend (..)
  , waitingDesc
  , submittedDesc
  , startedDesc
  , localProcessBackend

  -- Re-exports
  , Dict (..)
  , cap
  , cpure
  ) where

--------------------------------------------------------------------------------
import Data.Void
import System.IO
import System.Exit
import System.Process.Typed
import System.Environment
import Data.Binary
import Control.Concurrent
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Distributed.Closure
import Control.Concurrent.STM
--------------------------------------------------------------------------------

newtype ExecutorFailedException = ExecutorFailedException Text
  deriving Show
instance Exception ExecutorFailedException

execute :: Backend -> Closure (Dict (Serializable a)) -> Closure (IO a) -> IO a
execute b d c = do
  t <- spawnInternal b d c
  r <-
    liftIO . atomically $
    readTVar t >>= \case
      ExecutorPending _ -> retry
      ExecutorFinished a -> return a
  case r of
    ExecutorFailed err -> throwM $ ExecutorFailedException err
    ExecutorSucceeded a -> return a

spawn ::
   Backend
  ->  Closure (Dict (Serializable a))
  -> Closure (IO a)
  -> IO (IO (ExecutorStatus a))
spawn b d c = return . liftIO . readTVarIO =<< spawnInternal b d c

spawnInternal ::
   Backend
  ->   Closure (Dict (Serializable a))
  -> Closure (IO a)
  -> IO (TVar (ExecutorStatus a))
spawnInternal b d c = liftIO $ runBackend d c b

--------------------------------------------------------------------------------

data ExecutorPendingStatus
  = ExecutorWaiting (Maybe Text)
  | ExecutorSubmitted (Maybe Text)
  | ExecutorStarted (Maybe Text)

data ExecutorStatus a
  = ExecutorPending ExecutorPendingStatus
  | ExecutorFinished (ExecutorFinalStatus a)

data ExecutorFinalStatus a
  = ExecutorFailed Text
  | ExecutorSucceeded a

--------------------------------------------------------------------------------

data Backend = Backend
  { bExecute :: BL.ByteString -> BackendM BL.ByteString
  }

newtype BackendM a =
  BackendM (ReaderT (ExecutorPendingStatus -> IO ()) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

type ExecutorClosure = Closure (IO ())

toExecutorClosure :: Closure (Dict (Serializable a)) -> Closure (IO a) -> Closure (IO ())
toExecutorClosure dict cls =
  case unclosure dict of
    Dict -> static (\Dict i -> i >>= BL.putStr . encode) `cap` dict `cap` cls

parseAnswer :: Binary a => BL.ByteString -> ExecutorFinalStatus a
parseAnswer bs =
  case decodeOrFail bs of
    Left (_, _, err) -> ExecutorFailed $ "Error decoding answer: " <> T.pack err
    Right (_, _, a) -> ExecutorSucceeded a

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

pendingStatus :: ExecutorPendingStatus -> BackendM ()
pendingStatus s = BackendM ask >>= liftIO . ($ s)

waitingDesc :: Text -> BackendM ()
waitingDesc = pendingStatus . ExecutorWaiting . Just

submittedDesc :: Text -> BackendM ()
submittedDesc = pendingStatus . ExecutorSubmitted . Just

startedDesc :: Text -> BackendM ()
startedDesc = pendingStatus . ExecutorStarted . Just

--------------------------------------------------------------------------------

localProcessBackend :: Backend
localProcessBackend = Backend $ \stdin' -> do
  executable <- liftIO getExecutablePath
  let conf =
        setStdin (byteStringInput stdin') $
        proc executable [const_SERVERLESS_EXECUTOR_MODE]
  liftIO $ readProcessStdout_ conf

--------------------------------------------------------------------------------

const_SERVERLESS_EXECUTOR_MODE :: String
const_SERVERLESS_EXECUTOR_MODE = "SERVERLESS_EXECUTOR_MODE"

serverlessGuard :: IO ()
serverlessGuard = do
  getArgs >>= \case
    [x]
      | x == const_SERVERLESS_EXECUTOR_MODE -> vacuous runExecutor
    _ -> return ()

runExecutor :: IO Void
runExecutor = do
  BL.hGetContents stdin
    >>= unclosure . decode @ExecutorClosure
    >> exitWith ExitSuccess

--------------------------------------------------------------------------------
