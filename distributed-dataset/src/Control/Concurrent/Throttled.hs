{-# LANGUAGE RankNTypes #-}

-- |
-- A utility module which lets you put a concurrency limit to an IO action.
module Control.Concurrent.Throttled
  ( Throttle,
    newThrottle,
    throttled,
  )
where

--------------------------------------------------------------------------------
import Control.Concurrent.QSem
import Control.Monad.Catch
import Control.Monad.IO.Unlift

--------------------------------------------------------------------------------
data Throttle
  = Throttle
      { throttled :: forall m a. (MonadIO m, MonadMask m) => m a -> m a
      }

newThrottle :: MonadIO m => Int -> m Throttle
newThrottle n | n <= 0 = return $ Throttle id
newThrottle n = do
  sem <- liftIO $ newQSem n
  return $ Throttle $
    bracket_
      (liftIO $ waitQSem sem)
      (liftIO $ signalQSem sem)
