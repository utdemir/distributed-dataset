{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Distributed.Dataset.ShuffleStore
  ( ShuffleStore(..)
  , Range(..)
  ) where

-------------------------------------------------------------------------------
import           Conduit                     hiding (Consumer, Producer, await)
import           Control.Distributed.Closure
import           Data.Binary                 (Binary)
import           Data.ByteString             (ByteString)
import           Data.Int
import           GHC.Generics
-------------------------------------------------------------------------------

-- |
-- Provides a way to store intermediate temporary data.
--
-- See:
--
--   * 'Control.Distributed.Dataset.LocalTmpShuffleStore.withLocalTmpShuffleStore'
--
--   * <http://hackage.haskell.org/package/distributed-dataset-aws distributed-dataset-aws>
data ShuffleStore =
  ShuffleStore
    { ssGet   :: Closure (Int64 -> Range -> ConduitT () ByteString (ResourceT IO) ())
    , ssPut   :: Closure (Int64 -> ConduitT ByteString Void (ResourceT IO) ())
    }

data Range
  = RangeAll
  | RangeOnly Integer Integer
  deriving (Show, Eq, Generic, Binary)
