{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}

module Control.Distributed.Dataset.Aggr
  ( Aggr,
    aggrConst,
    aggrCount,
    aggrSum,
    aggrMean,
    aggrMax,
    aggrMin,
    aggrCollect,
    aggrDistinct,
    aggrTopK,
    aggrBottomK,
    aggrFiltered,

    -- * Creating Aggr's
    aggrFromMonoid,
    aggrFromReduce,
    aggrFromFold,
  )
where

import Control.Applicative.Static
import Control.Distributed.Closure
import Control.Distributed.Dataset.Internal.Aggr
import Control.Distributed.Dataset.Internal.Class
import qualified Control.Foldl as F
import Data.Functor.Static
import Data.HashSet (HashSet)
import qualified Data.Heap as H
import Data.List
import Data.Monoid
import Data.Ord
import Data.Profunctor.Static
import Data.Typeable

-- |
-- Returns the sum of the inputs.
aggrSum :: StaticSerialise a => Closure (Dict (Num a)) -> Aggr a a
aggrSum d =
  staticDimap
    (static Sum)
    (static getSum)
    (aggrFromMonoid $ static (\Dict -> Dict) `cap` d)

-- |
-- Returns the number of inputs.
aggrCount :: Typeable a => Aggr a Integer
aggrCount =
  static (const 1) `staticLmap` aggrSum (static Dict)

-- |
-- Calculates the mean of the inputs.
aggrMean :: Aggr Double Double
aggrMean =
  aggrConst (static (/))
    `staticApply` aggrSum (static Dict)
    `staticApply` staticMap (static realToFrac) aggrCount

-- |
-- Return the maximum of the inputs.

-- Returns 'Nothing' on empty 'Dataset's.
aggrMax :: StaticSerialise a => Closure (Dict (Ord a)) -> Aggr a (Maybe a)
aggrMax dict = aggrFromReduce $ static (\Dict -> max) `cap` dict

-- |
-- Return the minimum of the inputs.

-- Returns 'Nothing' on empty 'Dataset's.
aggrMin :: StaticSerialise a => Closure (Dict (Ord a)) -> Aggr a (Maybe a)
aggrMin dict = aggrFromReduce $ static (\Dict -> min) `cap` dict

-- |
-- Returns a new Aggr which only aggregates rows matching the predicate,
-- discarding others.
aggrFiltered :: Closure (a -> Bool) -> Aggr a b -> Aggr a b
aggrFiltered predc (Aggr f1 f2) =
  Aggr
    (static F.prefilter `cap` predc `cap` f1)
    f2

-- |
-- Collects the inputs as a list.

-- Warning: Ordering of the resulting list is non-deterministic.
aggrCollect :: StaticSerialise a => Aggr a [a]
aggrCollect =
  aggrFromFold
    (static F.list)
    (static (concat <$> F.list))

-- |
-- Collects the inputs to a 'HashSet'.
aggrDistinct :: forall a. (StaticSerialise a, StaticHashable a) => Aggr a (HashSet a)
aggrDistinct =
  aggrFromFold
    (static (\Dict -> F.hashSet) `cap` staticHashable @a)
    (static (\Dict -> mconcat <$> F.list) `cap` staticHashable @a)

-- * Top K

data TopK a = TopK Int (H.Heap a)
  deriving (Typeable)

instance Semigroup (TopK a) where
  TopK c1 h1 <> TopK c2 h2 =
    let m = min c1 c2
     in TopK m (H.drop (H.size h1 + H.size h2 - m) $ H.union h1 h2)

instance Monoid (TopK a) where
  mempty = TopK maxBound H.empty

-- |
-- Returns the 'n' greatest elements according to a key function. Similar to:
-- @take n . sortOn (Down . f)@

-- Warning: Ordering of the repeated elements is non-deterministic.
aggrTopK ::
  (StaticSerialise a, Typeable k) =>
  Closure (Dict (Ord k)) ->
  -- | Number of rows to return
  Int ->
  -- | Sorting key
  Closure (a -> k) ->
  Aggr a [a]
aggrTopK dict count fc =
  aggrFromFold
    ( static
        ( \Dict c f ->
            F.foldMap
              (\a -> TopK c . H.singleton $ H.Entry (f a) a)
              (\(TopK _ h) -> map H.payload . sortOn Down $ H.toUnsortedList h)
        )
        `cap` dict
        `cap` cpure (static Dict) count
        `cap` fc
    )
    ( static
        ( \Dict c f ->
            F.Fold (\a b -> take c $ merge f a b) [] id
        )
        `cap` dict
        `cap` cpure (static Dict) count
        `cap` fc
    )
  where
    merge _ xs [] = xs
    merge _ [] ys = ys
    merge f xss@(x : xs) yss@(y : ys) =
      if f x > f y
        then x : merge f xs yss
        else y : merge f xss ys

-- |
-- Returns the 'n' least elements according to a key function. Similar to:
-- @take n . sortOn (Down . f)@

-- Warning: Ordering of the repeated elements is non-deterministic.
aggrBottomK ::
  (StaticSerialise a, Typeable k) =>
  Closure (Dict (Ord k)) ->
  -- | Number of rows to return
  Int ->
  -- | Sorting key
  Closure (a -> k) ->
  Aggr a [a]
aggrBottomK d count fc =
  aggrTopK
    (static (\Dict -> Dict) `cap` d)
    count
    (static (Down .) `cap` fc)
