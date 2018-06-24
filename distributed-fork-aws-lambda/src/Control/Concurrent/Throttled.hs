-- |
-- A utility module which lets you put a concurrency limit to an IO action.
module Control.Concurrent.Throttled
  ( Throttle
  , newThrottle
  , noThrottle
  , throttled
  )  where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Control.Exception (bracket_)
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
--------------------------------------------------------------------------------

data Throttle
  = Throttle (TVar Int)
  | NoThrottle

newThrottle :: Int -> IO Throttle
newThrottle lim | lim <= 0 = return NoThrottle
newThrottle lim = Throttle <$> newTVarIO lim

noThrottle :: Throttle
noThrottle = NoThrottle

throttled :: MonadUnliftIO m => Throttle -> m a -> m a
throttled NoThrottle act = act
throttled (Throttle tv) act = do
  run <- askRunInIO
  liftIO $ bracket_
    (atomically $ do
       r <- readTVar tv
       when (r == 0) retry
       writeTVar tv (r - 1))
    (atomically $
       modifyTVar tv (+ 1))
    (run act)
