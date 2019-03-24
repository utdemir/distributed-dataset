{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Distributed.Dataset
  (
  -- * Dataset
    Dataset
  , DD
  , runDD
  , runDDWith
  , Backend
  , ShuffleStore
  , dPartition
  , dExternal
  , Partition
  , mkPartition
  -- * Transformations
  , dMap
  , dFilter
  , dToList
  , dPipe
  , dConcatMap
  , dCoalesce
  , dFetch
  -- * Aggregations
  , dAggr
  , dGroupedAggr
  , dCount
  , dSum
  , dAvg
  , dCollect
  , dTopK
  , dBottomK
  , Aggr(..)
  -- * Class
  , StaticSerialise(..)
  , StaticHashable(..)
  -- * Utilities
  , (&)
  -- * Re-exports
  , initDistributedFork
  , liftIO
  , Serialise
  -- * Closure
  , Serializable
  , Closure
  , cap
  , cpure
  , Dict (Dict)
  ) where

-------------------------------------------------------------------------------
import           Codec.Serialise
import           Conduit                                      hiding (Consumer,
                                                               Producer, await)
import qualified Conduit                                      as C
import           Control.Distributed.Closure
import           Control.Distributed.Fork
import qualified Control.Foldl                                as F
import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict                          as HM
import           Data.Typeable
import           Data.Void
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Aggr
import           Control.Distributed.Dataset.Internal.Class
import           Control.Distributed.Dataset.Internal.Dataset
import           Control.Distributed.Dataset.ShuffleStore
import           Data.Conduit.Foldl
-------------------------------------------------------------------------------

mkPartition :: Typeable a => Closure (ConduitT () a (ResourceT IO) ()) -> Partition a
mkPartition = PSimple

-- * Dataset

-- |
-- Transforms a 'Dataset' by passing every partition through the given Conduit.
dPipe :: (StaticSerialise a, StaticSerialise b)
      => Closure (ConduitT a b (ResourceT IO) ())
      -> Dataset a -> Dataset b
dPipe = DPipe

-- |
-- Create a dataset from given 'Partition'''s.
--
-- This is how every 'Dataset' is created initially.
dExternal :: Typeable a => [Partition a] -> Dataset a
dExternal = DExternal

-- |
-- Re-partition the dataset using the given function so that the rows with the same 'k' will
-- end up in the same partition.
dPartition :: (StaticSerialise a, StaticHashable k) => Int -> Closure (a -> k) -> Dataset a -> Dataset a
dPartition = DPartition

-- |
-- Re-partition the dataset using the given function so that the rows with the same 'k' will
-- end up in the same partition.
dCoalesce :: Typeable a => Int -> Dataset a -> Dataset a
dCoalesce = DCoalesce

-- * Dataset API

dConcatMap :: (StaticSerialise a, StaticSerialise b) => Closure (a -> [b]) -> Dataset a -> Dataset b
dConcatMap f_ = dPipe $ static (C.concatMapC @(ResourceT IO)) `cap` f_

dMap :: (StaticSerialise a, StaticSerialise b) => Closure (a -> b) -> Dataset a -> Dataset b
dMap f = dConcatMap $ static (pure .) `cap` f

dFilter :: (StaticSerialise a) => Closure (a -> Bool) -> Dataset a -> Dataset a
dFilter f = dConcatMap $ static (\f_ a -> if f_ a then [a] else []) `cap` f

-- |
-- Apply an aggregation to all items on a Dataset, and fetch the result.
--
-- Every partition will be reduced remotely, and the results will be reduced on the driver.
dAggr :: forall a b. (StaticSerialise a, StaticSerialise b) => Aggr a b -> Dataset a -> DD b
dAggr (Aggr f1c f2c) ds = do
  c <- ds
         & dPipe (static (\f1 ->
             (C.mapOutput absurd (foldConduit f1)) >>= C.yield) `cap` f1c
           )
         & dFetch
  liftIO . runConduitRes $ c .| foldConduit (unclosure f2c)

-- |
-- Apply an aggregation to all items sharing the same key on a Dataset.
--
-- All computation will happen on the executors.
dGroupedAggr :: forall k a b.
                ( StaticHashable k, StaticSerialise k
                , StaticSerialise a, StaticSerialise b
                )
             => Int              -- ^ Number of partitions
             -> Closure (a -> k) -- ^ Key function
             -> Aggr a b
             -> Dataset a
             -> Dataset (k, b)
dGroupedAggr
  partitionCount
  kc
  (Aggr
    (f1c :: Closure (F.Fold a t))
    (f2c :: Closure (F.Fold t b))
  ) ds
  = ds
      & dMap (static (\k a -> (k a, a)) `cap` kc)
      & dPipe (static (\Dict -> aggrC @a @t @k)
                `cap` staticHashable @k `cap` f1c)
      & dPartition partitionCount (static (fst @k @t))
      & dPipe (static (\Dict -> aggrC @t @b @k)
                `cap` staticHashable @k `cap` f2c)
  where
    aggrC :: forall a' b' k'. (Eq k', Hashable k')
          => F.Fold a' b'
          -> ConduitT (k', a') (k', b') (ResourceT IO) ()
    aggrC (F.Fold step init extract) =
      go step init extract HM.empty

    go :: forall a' b' x' k'. (Eq k', Hashable k')
       => (b' -> a' -> b') -> b' -> (b' -> x')
       -> HM.HashMap k' b'
       -> ConduitT (k', a') (k', x') (ResourceT IO) ()
    go step init extract hm = C.await >>= \case
      Nothing ->
        mapM_
          (\(k, b) -> C.yield (k, extract b))
          (HM.toList hm)
      Just (k, a) ->
        go step init extract $ HM.alter
          (Just . \case
             Nothing -> step init a
             Just st -> step st a
          )
          k
          hm
