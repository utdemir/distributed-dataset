{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Control.Distributed.Dataset.Internal.Class where

-------------------------------------------------------------------------------
import           Codec.Serialise
import           Control.Distributed.Closure
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Hashable
import           Data.Monoid
import           Data.Ord
import           Data.Text
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.ShuffleStore
import           Control.Distributed.Fork                 (Backend)
-------------------------------------------------------------------------------

data DDEnv = DDEnv
  { _ddBackend      :: Backend
  , _ddShuffleStore :: ShuffleStore
  , _ddLogLevel     :: LogLevel
  }

makeLenses ''DDEnv

newtype DD a = DDT (ReaderT DDEnv (LoggingT IO) a)
  deriving ( Functor, Applicative, Monad, MonadReader DDEnv
           , MonadIO, MonadThrow, MonadCatch, MonadLogger
           , MonadUnliftIO
           )

runDD :: Backend -> ShuffleStore -> DD a -> IO a
runDD = runDDWith LevelInfo

runDDWith :: LogLevel -> Backend -> ShuffleStore -> DD a -> IO a
runDDWith level backend shuffleStore (DDT act) = do
  let env = DDEnv backend shuffleStore level
      logFilter = filterLogger $ \_ l -> l >= level
  runStderrLoggingT $ logFilter $ runReaderT act env

-------------------------------------------------------------------------------

class (Typeable a, Serialise a) => StaticSerialise a where
   staticSerialise :: Closure (Dict (Typeable a, Serialise a))

instance StaticSerialise () where
   staticSerialise = static Dict
instance StaticSerialise Bool where
   staticSerialise = static Dict
instance StaticSerialise Char where
   staticSerialise = static Dict
instance StaticSerialise Double where
   staticSerialise = static Dict
instance StaticSerialise Float where
   staticSerialise = static Dict
instance StaticSerialise Int where
   staticSerialise = static Dict
instance StaticSerialise Integer where
   staticSerialise = static Dict
instance StaticSerialise Text where
   staticSerialise = static Dict
instance StaticSerialise a => StaticSerialise [a] where
   staticSerialise = static (\Dict -> Dict) `cap` staticSerialise @a
instance StaticSerialise a => StaticSerialise (Maybe a) where
   staticSerialise = static (\Dict -> Dict) `cap` staticSerialise @a
instance StaticSerialise a => StaticSerialise (Sum a) where
   staticSerialise = static (\Dict -> Dict) `cap` staticSerialise @a
instance StaticSerialise a => StaticSerialise (Down a) where
   staticSerialise = static (\Dict -> Dict) `cap` staticSerialise @a
instance (StaticSerialise a, StaticSerialise b) => StaticSerialise (a, b) where
   staticSerialise = static (\Dict Dict -> Dict) `cap` staticSerialise @a `cap` staticSerialise @b
instance (StaticSerialise a, StaticSerialise b, StaticSerialise c) => StaticSerialise (a, b, c) where
   staticSerialise = static (\Dict Dict Dict -> Dict) `cap` staticSerialise @a `cap` staticSerialise @b `cap` staticSerialise @c


class (Typeable a, Eq a, Hashable a) => StaticHashable a where
   staticHashable :: Closure (Dict (Typeable a, Eq a, Hashable a))

instance StaticHashable () where
   staticHashable = static Dict
instance StaticHashable Bool where
   staticHashable = static Dict
instance StaticHashable Char where
   staticHashable = static Dict
instance StaticHashable Double where
   staticHashable = static Dict
instance StaticHashable Float where
   staticHashable = static Dict
instance StaticHashable Int where
   staticHashable = static Dict
instance StaticHashable Integer where
   staticHashable = static Dict
instance StaticHashable Text where
   staticHashable = static Dict
instance StaticHashable a => StaticHashable [a] where
   staticHashable = static (\Dict -> Dict) `cap` staticHashable @a
instance (StaticHashable a, StaticHashable b) => StaticHashable (a, b) where
   staticHashable = static (\Dict Dict -> Dict) `cap` staticHashable @a `cap` staticHashable @b
instance (StaticHashable a, StaticHashable b, StaticHashable c) => StaticHashable (a, b, c) where
   staticHashable = static (\Dict Dict Dict -> Dict) `cap` staticHashable @a `cap` staticHashable @b `cap` staticHashable @c
