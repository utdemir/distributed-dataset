{-# LANGUAGE RankNTypes #-}

-- |
-- A utility module which lets you put a concurrency limit to an IO action.
module Control.Concurrent.Throttled
  ( Throttle
  , newThrottle
  , throttled
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.QSem
import           Control.Exception       (bracket_)
import           Control.Monad.IO.Unlift
--------------------------------------------------------------------------------

data Throttle = Throttle { throttled :: forall m a. MonadUnliftIO m => m a -> m a }

newThrottle :: MonadIO m => Int -> m Throttle
newThrottle n | n <= 0 = return $ Throttle id
newThrottle n = do
  sem <- liftIO $ newQSem n
  return $ Throttle $ \ma -> do
    run <- askUnliftIO
    liftIO
      $ bracket_  (waitQSem sem) (signalQSem sem)
      $ unliftIO run ma
