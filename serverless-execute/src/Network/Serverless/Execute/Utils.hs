{-# LANGUAGE LambdaCase #-}

module Network.Serverless.Execute.Utils where

--------------------------------------------------------------------------------
import Control.Monad (forM, unless, forever)
import Control.Concurrent.STM (newTVar, atomically, retry, readTVar, modifyTVar, writeTVar)
import Control.Monad.Trans.State (evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (async, wait, cancel)
import Control.Exception (throwIO)
import Data.Function (fix)
import Control.Concurrent (threadDelay)
--------------------------------------------------------------------------------
import Network.Serverless.Execute
--------------------------------------------------------------------------------

mapWithProgress :: Backend
                -> Closure (Dict (Serializable a))
                -> [Closure (IO a)]
                -> IO [a]
mapWithProgress backend dict xs = do
  st <- atomically $ newTVar (True, 0::Int, 0::Int, 0::Int, 0::Int)
  tvars <- mapM (executeAsync backend dict) xs
  asyncs <- forM tvars $ \tv ->
    async . flip evalStateT (0, 0, 0, 0) . fix $ \f -> do
      oldState <- get
      (newState, result) <- liftIO $ atomically $ do
        (n, r) <- readTVar tv >>= return . \case
          ExecutorPending (ExecutorWaiting _)   -> ((1, 0, 0, 0), Nothing)
          ExecutorPending (ExecutorSubmitted _) -> ((0, 1, 0, 0), Nothing)
          ExecutorPending (ExecutorStarted _)   -> ((0, 0, 1, 0), Nothing)
          ExecutorFinished fr                   -> ((0, 0, 0, 1), Just fr)
        unless (oldState /= n) retry
        return (n, r)
      put newState
      liftIO . atomically $
        modifyTVar st $ \case
          (_, s1, s2, s3, s4) -> case (oldState, newState) of
            ((o1, o2, o3, o4), (n1, n2, n3, n4)) ->
              (True, s1 - o1 + n1, s2 - o2 + n2, s3 - o3 + n3, s4 - o4 + n4)
      case result of
        Nothing -> f
        Just (ExecutorFailed err) -> liftIO . throwIO $ ExecutorFailedException err
        Just (ExecutorSucceeded x) -> return x

  let total = length xs
  pbar <- async . forever $ do
    (waiting, submitted, started, finished) <- atomically $ do
      readTVar st >>= \case
        (False, _, _, _, _) -> retry
        (True, a, b, c, d) -> do
          writeTVar st (False, a, b, c, d)
          return (a, b, c, d)
    print (total, waiting, submitted, started, finished)
    threadDelay 10000

  results <- mapM wait asyncs
  cancel pbar
  return results
