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
  ( Dataset
  -- * Transformations
  , dMap
  , dFilter
  , dConcatMap
  , dGroupedAggr
  , dDistinct
  -- ** Low-level transformations
  , dCoalesce
  , dPipe
  , dPartition
  -- * Aggregations
  , module Control.Distributed.Dataset.Aggr
  -- * Execution
  , dAggr
  , dFetch
  , dToList
  , DD
  , runDD
  , runDDWith
  , Backend
  , ShuffleStore
  -- * Creating datasets
  , Partition
  , mkPartition
  , dExternal
  -- * Class
  , StaticSerialise(..)
  , StaticHashable(..)
  -- * Re-exports
  , initDistributedFork
  , liftIO
  , (&)
  -- ** Closure
  , Closure
  , cap
  , cpure
  , Dict (Dict)
  ) where

-------------------------------------------------------------------------------
import           Conduit                                      hiding (Consumer,
                                                               Producer, await)
import qualified Conduit                                      as C
import           Control.Distributed.Closure
import           Control.Distributed.Fork
import qualified Control.Foldl                                as F
import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict                          as HM
import           Data.Profunctor.Static
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Aggr
import           Control.Distributed.Dataset.Internal.Aggr
import           Control.Distributed.Dataset.Internal.Class
import           Control.Distributed.Dataset.Internal.Dataset
import           Control.Distributed.Dataset.ShuffleStore
-------------------------------------------------------------------------------

-- |
-- Create a 'Partition' given a Source conduit.
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
-- Create a dataset from given `Partition`'s.
--
-- This is how every 'Dataset' is created initially.
dExternal :: StaticSerialise a => [Partition a] -> Dataset a
dExternal = DExternal

-- |
-- Re-partition the dataset using the given function so that the items with the same 'k' will
-- end up in the same partition.
dPartition :: (StaticSerialise a, StaticHashable k) => Int -> Closure (a -> k) -> Dataset a -> Dataset a
dPartition = DPartition

-- |
-- Coalesce partitions together to get the specified number of partitions.
dCoalesce :: Typeable a => Int -> Dataset a  -> Dataset a
dCoalesce = DCoalesce

-- * Dataset API

-- |
-- Returns a new Dataset by first applying a function to all elements of this Dataset, and then
-- flattening the results.
dConcatMap :: (StaticSerialise a, StaticSerialise b) => Closure (a -> [b]) -> Dataset a -> Dataset b
dConcatMap f = dPipe $ static (C.concatMapC @(ResourceT IO)) `cap` f

-- |
-- Returns a new Dataset that contains the result of applying the given function to each element.
dMap :: (StaticSerialise a, StaticSerialise b) => Closure (a -> b) -> Dataset a -> Dataset b
dMap f = dConcatMap $ static (pure .) `cap` f

-- |
-- Returns a new Dataset that only contains elements where the given function returns true.
dFilter :: (StaticSerialise a) => Closure (a -> Bool) -> Dataset a -> Dataset a
dFilter f = dConcatMap $ static (\f_ a -> if f_ a then [a] else []) `cap` f

-- |
-- Apply an aggregation to all items on a Dataset, and fetch the result.
dAggr :: (StaticSerialise a, StaticSerialise b) => Aggr a b -> Dataset a -> DD b
dAggr aggr@(Aggr _ fc) ds = do
  c <- ds
         & dGroupedAggr 1 (static (const ())) aggr
         & dFetch
  liftIO (runConduitRes $ c .| C.await) >>= return . \case
    Just ((), r) -> r
    Nothing -> F.fold (unclosure fc) []


-- |
-- Removes a new dataset with duplicate rows removed.
dDistinct :: StaticHashable a
          => Int -- ^ Target number of partitions
          -> Dataset a
          -> Dataset a
dDistinct partitionCount ds =
  case dStaticSerialise ds of
    Dict ->
      dGroupedAggr
        partitionCount
        (static id)
        (staticLmap (static (const ())) $ aggrFromMonoid (static Dict))
        ds
        & dMap (static fst)

-- |
-- Apply an aggregation to all rows sharing the same key.
dGroupedAggr :: ( StaticHashable k, StaticSerialise k , StaticSerialise b)
             => Int              -- ^ Target number of partitions
             -> Closure (a -> k) -- ^ Grouping key
             -> Aggr a b
             -> Dataset a
             -> Dataset (k, b)
dGroupedAggr
  partitionCount
  (kc :: Closure (a -> k))
  (Aggr
    (f1c :: Closure (F.Fold a t))
    (f2c :: Closure (F.Fold t b))
  ) ds
  = case dStaticSerialise ds of
      Dict -> ds
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
