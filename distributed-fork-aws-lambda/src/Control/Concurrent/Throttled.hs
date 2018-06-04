{-# LANGUAGE DeriveGeneric #-}

-- |
-- A utility module which lets you put a concurrency limit to an IO action.
module Control.Concurrent.Throttled
  ( Throttle
  , newThrottle
  , throttled
  )  where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Control.Exception (finally)
import Control.Concurrent.STM
--------------------------------------------------------------------------------

newtype Throttle
  = Throttle (TVar Int)

newThrottle :: Int -> IO Throttle
newThrottle lim =
  Throttle <$> newTVarIO (max lim 1)

throttled :: Throttle -> IO a -> IO a
throttled (Throttle tv) act = do
  atomically $ do
    r <- readTVar tv
    when (r == 0) retry
    writeTVar tv (r - 1)
  finally act $
    atomically $ modifyTVar tv (+ 1)
