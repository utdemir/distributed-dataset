-- |
-- This module provides a common interface for offloading an IO action to remote executors.
--
-- It uses
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#static-pointers StaticPointers language extension> and
-- <https://hackage.haskell.org/package/distributed-closure distributed-closure> library
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
--     * If @a@ is a runtime value, use 'cpure' to lift it to @Closure a@. It will ask
--   for a @('Closure' ('Dict' ('Serializable' a)))@. If there is @('Binary' a)@ and
--   @('Typeable' a)@ instances, you can just use @(static 'Dict')@ for that.
--
-- One important constraint when using this library is that it assumes the remote
-- environment is capable of executing the exact same binary. On most cases, this
-- requires your host environment to be Linux. In future I plan to provide a set
-- of scripts using Docker to overcome this limitation.
module Control.Distributed.Fork
  ( fork,
    initDistributedFork,
    Backend,

    -- * Handle
    Handle,
    await,

    -- * ExecutorStatus
    pollHandle,
    ExecutorStatus (..),
    ExecutorPendingStatus (..),
    ExecutorFinalStatus (..),

    -- * Exceptions
    ExecutorFailedException (..),

    -- * Re-exports
    Serializable,
    Closure,
    cap,
    cpure,
    Dict (Dict),
  )
where

--------------------------------------------------------------------------------
import Control.Distributed.Closure
--------------------------------------------------------------------------------
import Control.Distributed.Fork.Internal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text)

--------------------------------------------------------------------------------

-- |
-- Asynchronously executes the given function using the 'Backend' and returns
-- an 'Handle'.
--
-- @
-- {-\# LANGUAGE StaticPointers #-}
--
-- import Control.Distributed.Fork
-- import Control.Distributed.Fork.Local
--
-- main :: IO ()
-- main = do
--   'initDistributedFork'
--   handle <- 'fork' 'Control.Distributed.Fork.Local.localProcessBackend' (static 'Dict') (static (return "Hello World!"))
--   await handle >>= putStrLn
-- @
fork ::
  MonadIO m =>
  Backend ->
  Closure (Dict (Serializable a)) ->
  Closure (IO a) ->
  m (Handle a)
fork b d c = liftIO $ runBackend d c b

-- |
-- Blocks until the 'Handle' completes.
--
-- Can throw 'ExecutorFailedException'.
await :: (MonadIO m, MonadThrow m) => Handle a -> m a
await = liftIO . tryAwait >=> either (throwM . ExecutorFailedException) return

--------------------------------------------------------------------------------
newtype ExecutorFailedException = ExecutorFailedException Text
  deriving (Show, Eq)

instance Exception ExecutorFailedException
