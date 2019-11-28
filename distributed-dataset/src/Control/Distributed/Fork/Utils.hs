{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Control.Distributed.Fork.Utils
  ( forkConcurrently,
    Options (..),
    defaultOptions,
  )
where

--------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, waitBoth)
import Control.Concurrent.STM
--------------------------------------------------------------------------------
import Control.Distributed.Fork
import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Retry
import Data.Function (fix)
import Data.Functor
import Data.Group
import qualified System.Console.Terminal.Size as TS

--------------------------------------------------------------------------------
data Options
  = Options
      { oRetries :: Int,
        oShowProgress :: Bool
      }

defaultOptions :: Options
defaultOptions = Options 2 True

data Progress
  = Progress
      { waiting :: Int,
        submitted :: Int,
        started :: Int,
        finished :: Int,
        retried :: Int
      }
  deriving (Eq)

instance Semigroup Progress where
  Progress a1 b1 c1 d1 e1 <> Progress a2 b2 c2 d2 e2 =
    Progress (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)

instance Monoid Progress where
  mempty = Progress 0 0 0 0 0

instance Group Progress where
  invert (Progress a b c d e) =
    Progress (negate a) (negate b) (negate c) (negate d) (negate e)

-- |
-- Runs given closures concurrently using the 'Backend', showing a
-- progress bar.
--
-- Throws 'Execut orFailedException' if something fails.
forkConcurrently ::
  Options ->
  Backend ->
  Closure (Dict (Serializable a)) ->
  [Closure (IO a)] ->
  IO [a]
forkConcurrently options backend dict xs = do
  st <- atomically $ newTVar (True, mempty)
  asyncs <- forM xs $ updateThread st
  result <- async $ mapM wait asyncs
  pbar <-
    if oShowProgress options
      then progressThread st (length xs)
      else async $ return ()
  fst <$> waitBoth result pbar
  where
    updateThread st act =
      async $ do
        progress <- newTVarIO mempty
        recoverAll (limitRetries $ oRetries options) $ \retryStatus -> do
          handle <- fork backend dict act
          fix $ \recurse -> do
            res <-
              liftIO $ atomically $ do
                old <- readTVar progress
                let base = mempty {retried = rsIterNumber retryStatus}
                (new, res) <-
                  pollHandle handle <&> \case
                    ExecutorPending (ExecutorWaiting _) -> (base {waiting = 1}, Nothing)
                    ExecutorPending (ExecutorSubmitted _) -> (base {submitted = 1}, Nothing)
                    ExecutorPending (ExecutorStarted _) -> (base {started = 1}, Nothing)
                    ExecutorFinished fr -> (base {finished = 1}, Just fr)
                when (old == new) retry
                writeTVar progress new
                modifyTVar st $ \case
                  (_, m) -> (True, m <> invert old <> new)
                return res
            case res of
              Just (ExecutorFailed err) -> liftIO . throwIO $ ExecutorFailedException err
              Just (ExecutorSucceeded x) -> return x
              Nothing -> recurse
    progressThread st total = do
      termWidth <- maybe 40 TS.width <$> TS.size :: IO Int
      let ratio = fromIntegral total / fromIntegral (termWidth - 2) :: Double
      async . fix $ \recurse -> do
        progress <-
          atomically $
            readTVar st
              >>= \case
                (False, _) -> retry
                (True, p) -> do
                  writeTVar st (False, p)
                  return p
        let p c n = replicate (truncate (fromIntegral n / ratio)) c
        putStr . concat $
          [ "\r",
            "[",
            p '#' (finished progress),
            p ':' (started progress),
            p '.' (submitted progress),
            p ' ' (waiting progress),
            "]"
          ]
        if finished progress < total
          then threadDelay 10000 >> recurse
          else putStrLn ""
