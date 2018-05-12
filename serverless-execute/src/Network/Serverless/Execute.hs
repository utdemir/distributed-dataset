{-# LANGUAGE LambdaCase #-}

-- |
-- This module provides a common interface for executing IO actions on FaaS (Function
-- as a Service) providers like <https://aws.amazon.com/lambda AWS Lambda>.
--
-- It uses @StaticPointers@ language extension and 'distributed-closure' library
-- for serializing closures to run remotely.
-- <https://ocharles.org.uk/blog/guest-posts/2014-12-23-static-pointers.html This blog post>
-- is a good introduction for those.
--
-- In short, if you you need a @'Closure' a@:
--
--     * If @a@ is statically known (eg. a top level value, or if it does not
--   depend on anything on the scope), use @static@ keyword coming from
--   @StaticPointers@ extension.
--
--     * If @a@ is a runtime value, use 'cpure' to lift it to 'Closure a'. It will ask
--   for a @('Closure' ('Dict' ('Serializable' a)))@. If there is @('Binary' a)@ and
--   @('Typeable' a)@ instances, you can just use @(static 'Dict')@ for that.
--
-- One important constraint when using this library is that it assumes the remote
-- environment is capable of executing the exact same binary. On most cases, this
-- requires your host environment to be Linux. In future I plan to provide a set
-- of scripts using Docker to overcome this limitation.
module Network.Serverless.Execute
  ( execute
  , initServerless
  , Backend

  -- * Status
  , executeAsync
  , ExecutorStatus (..)
  , ExecutorPendingStatus (..)
  , ExecutorFinalStatus (..)

  -- * Exceptions
  , ExecutorFailedException (..)

  -- * Re-exports
  , Serializable
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
-- Executes the given function using the 'Backend'.
--
-- Can throw 'ExecutorFailedException'.
--
-- @
-- {-\# LANGUAGE StaticPointers #-}
--
-- import Network.Serverless.Execute
-- import Network.Serverless.Execute.LocalProcessBackend
--
-- main :: IO ()
-- main = do
--   'initServerless'
--   ret <- 'execute' 'localProcessBackend' (static 'Dict') (static (return "Hello World!"))
--   putStrLn ret
-- @
execute :: Backend
           -- ^ Backend to execute the function.
        -> Closure (Dict (Serializable a))
           -- ^ A static evidence for @Serializable a@.
           --   On most cases, just @(static Dict)@ is enough.
        -> Closure (IO a)
           -- ^ Function to execute.
        -> IO a
execute b d c = do
  t <- executeAsync b d c
  r <-
    liftIO . atomically $
    readTVar t >>= \case
      ExecutorPending _ -> retry
      ExecutorFinished a -> return a
  case r of
    ExecutorFailed err  -> throwM $ ExecutorFailedException err
    ExecutorSucceeded a -> return a

-- |
-- Same as 'execute', but immediately returns with a TVar containing the state
-- of the executor.
executeAsync :: Backend
             -> Closure (Dict (Serializable a))
             -> Closure (IO a)
             -> IO (TVar (ExecutorStatus a))
executeAsync b d c = runBackend d c b

--------------------------------------------------------------------------------

newtype ExecutorFailedException = ExecutorFailedException Text
  deriving (Show, Eq)
instance Exception ExecutorFailedException
