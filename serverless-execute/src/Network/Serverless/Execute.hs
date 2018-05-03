{-# LANGUAGE LambdaCase #-}

module Network.Serverless.Execute
  ( Backend
  , initServerless
  , execute

  -- * Exceptions
  , ExecutorFailedException

  -- * Re-exports
  , Closure
  , cap
  , cpure
  , Dict (Dict)
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Distributed.Closure
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Text                           (Text)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute.Internal
--------------------------------------------------------------------------------

-- |
-- Runs the given action on the 'Backend'.
--
-- Can throw 'ExecutorFailedException'.
execute :: Backend
           -- ^ Backend to execute the function.
        -> Closure (Dict (Serializable a))
           -- ^ A static evidence for @Serializable a@.
           --   On most cases, just @(static Dict)@ is enough.
        -> Closure (IO a)
           -- ^ Function to execute.
        -> IO a
execute b d c = do
  t <- spawnInternal b d c
  r <-
    liftIO . atomically $
    readTVar t >>= \case
      ExecutorPending _ -> retry
      ExecutorFinished a -> return a
  case r of
    ExecutorFailed err  -> throwM $ ExecutorFailedException err
    ExecutorSucceeded a -> return a

spawnInternal ::
     Backend
  -> Closure (Dict (Serializable a))
  -> Closure (IO a)
  -> IO (TVar (ExecutorStatus a))
spawnInternal b d c = liftIO $ runBackend d c b

--------------------------------------------------------------------------------

newtype ExecutorFailedException = ExecutorFailedException Text
  deriving Show
instance Exception ExecutorFailedException
