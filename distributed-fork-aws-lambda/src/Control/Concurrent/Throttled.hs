module Control.Concurrent.Throttled
  ( Throttle
  , newThrottle
  , throttled
  )  where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Exception
import Control.Concurrent
--------------------------------------------------------------------------------

data Throttle
  = Throttle (MVar (MVar ()))

newThrottle :: Int -> IO Throttle
newThrottle maxConcurrent = do
  mv <- newEmptyMVar
  forM_ [1..maxConcurrent] $ \_ -> forkIO . forever $ do
    m <- takeMVar mv
    putMVar m ()
    putMVar m ()
  return $ Throttle mv

throttled :: Throttle -> IO a -> IO a
throttled (Throttle mv) act = do
  m <- newEmptyMVar
  putMVar mv m
  readMVar m
  act `finally` takeMVar m
