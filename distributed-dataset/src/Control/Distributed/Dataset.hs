{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

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
  , Aggr(..)
  -- * Class
  , StaticSerialise(..)
  , StaticHashable(..)
  , CommutativeSemigroup
  , CommutativeMonoid
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
import           Control.Lens
import qualified Data.HashMap.Strict                          as HM
import           Data.Monoid.Commutative
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Aggr
import           Control.Distributed.Dataset.Internal.Class
import           Control.Distributed.Dataset.Internal.Dataset
import           Control.Distributed.Dataset.ShuffleStore
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
dConcatMap f_ = dPipe $ static (\f -> C.concatMapC @(ResourceT IO) f) `cap` f_

dMap :: (StaticSerialise a, StaticSerialise b) => Closure (a -> b) -> Dataset a -> Dataset b
dMap f = dConcatMap $ static (pure .) `cap` f

dFilter :: (StaticSerialise a) => Closure (a -> Bool) -> Dataset a -> Dataset a
dFilter f = dConcatMap $ static (\f_ a -> if f_ a then [a] else []) `cap` f

-- |
-- Apply an aggregation to all items on a Dataset, and fetch the result.
--
-- Every partition will be reduced remotely, and the results will be reduced on the driver.
dAggr :: (StaticSerialise a, StaticSerialise b) => Aggr a b -> Dataset a -> DD b
dAggr (Aggr (m :: Closure (a -> t)) r e) ds = do
  c <- ds
         & dMap m
         & dPipe (static (\Dict -> go @t mempty) `cap` r)
         & dFetch
  case unclosure r of
    Dict -> do
      t <- liftIO . runConduitRes $ c .| C.foldMapC id
      return $ unclosure e t
 where
   go :: forall a' m. (CommutativeSemigroup a', Monad m) => a' -> ConduitT a' a' m ()
   go !i =
     C.await >>= \case
       Nothing -> C.yield i
       Just a  -> go (i <> a)

-- |
-- Apply an aggregation to all items sharing the same key on a Dataset.
--
-- All computation will happen on the executors.
dGroupedAggr :: forall k v r.
                ( StaticHashable k, StaticSerialise k
                , StaticSerialise v, StaticSerialise r
                )
             => Int              -- ^ Number of partitions
             -> Closure (v -> k) -- ^ Key function
             -> Aggr v r
             -> Dataset v
             -> Dataset (k, r)
dGroupedAggr
 partitionCount
 keyC
 (Aggr (m :: Closure (v -> t))
       (r :: Closure (Dict (CommutativeMonoid t)))
       (e :: Closure (t -> r))
 )
 ds
  = ds
      & dMap @_ @(k, t) (static (\key m' v -> (key v, m' v)) `cap` keyC `cap` m)
      & dPipe bucket
      & dPartition partitionCount (static (fst @k @t))
      & dPipe bucket
      & dMap (static (\e' (k, v) -> (k, e' v)) `cap` e)
  where
    bucket :: Closure (ConduitT (k, t) (k, t) (ResourceT IO) ())
    bucket = static (\Dict Dict -> go HM.empty) `cap` r `cap` staticHashable @k

    go hm = C.await >>= \case
      Nothing -> mapM_ C.yield (HM.toList hm)
      Just (k, v) -> go $ HM.insertWith (<>) k v hm
