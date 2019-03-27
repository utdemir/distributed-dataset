{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Distributed.Dataset.Aggr
  ( Aggr
  , dConstAggr
  , dCount
  , dSum
  , dMean
  , dMax
  , dMin
  , dCollect
  , dDistinct
  , dBottomK
  , dTopK
  , dFilteredAggr
  , aggrFromMonoid
  , aggrFromFold
  ) where

-------------------------------------------------------------------------------
import           Control.Applicative.Static
import           Control.Distributed.Closure
import qualified Control.Foldl                              as F
import           Data.Functor.Static
import           Data.HashSet                               (HashSet)
import qualified Data.Heap                                  as H
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Profunctor.Static
import           Data.Semigroup
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Aggr
import           Control.Distributed.Dataset.Internal.Class
-------------------------------------------------------------------------------

dSum :: StaticSerialise a => Closure (Dict (Num a)) -> Aggr a a
dSum d =
  staticDimap
    (static Sum)
    (static getSum)
    (aggrFromMonoid $ static (\Dict -> Dict) `cap` d)

dCount :: Typeable a => Aggr a Integer
dCount =
  static (const 1) `staticLmap` dSum (static Dict)

dMean :: Aggr Double Double
dMean =
  dConstAggr (static (/))
    `staticApply` dSum (static Dict)
    `staticApply` staticMap (static realToFrac) dCount

dMax :: StaticSerialise a => Closure (Dict (Ord a)) -> Aggr a (Maybe a)
dMax dict =
  staticDimap
    (static (Just . Max))
    (static (fmap getMax))
    (aggrFromMonoid (static (\Dict -> Dict) `cap` dict))

dMin :: StaticSerialise a => Closure (Dict (Ord a)) -> Aggr a (Maybe a)
dMin dict =
  staticDimap
    (static (Just . Min))
    (static (fmap getMin))
    (aggrFromMonoid (static (\Dict -> Dict) `cap` dict))

-- |
-- Returns a new Aggr which only aggregates rows matching the predicate.
dFilteredAggr :: Closure (a -> Bool) -> Aggr a b -> Aggr a b
dFilteredAggr predc (Aggr f1 f2) =
  Aggr
    (static F.prefilter `cap` predc `cap` f1)
    f2

-- * Collect

-- |
-- Warning: Ordering of the resulting list is non-deterministic.
dCollect :: StaticSerialise a => Aggr a [a]
dCollect =
  aggrFromFold
    (static F.list)
    (static (concat <$> F.list))

dDistinct :: forall a. (StaticSerialise a, StaticHashable a) => Aggr a (HashSet a)
dDistinct =
  aggrFromFold
    (static (\Dict -> F.hashSet) `cap` staticHashable @a)
    (static (\Dict -> mconcat <$> F.list) `cap` staticHashable @a)

-- * Top K

data TopK a = TopK Int (H.Heap a)
  deriving (Typeable)

instance Semigroup (TopK a) where
  TopK c1 h1 <> TopK c2 h2 =
    let m = min c1 c2
    in  TopK m (H.drop (H.size h1 + H.size h2 - m) $ H.union h1 h2)

instance Monoid (TopK a) where
  mempty = TopK maxBound H.empty

dTopK :: forall a k. (StaticSerialise a, Typeable k)
      => Closure (Dict (Ord k))
      -> Int              -- ^ Number of rows to return
      -> Closure (a -> k) -- ^ Sorting key
      -> Aggr a [a]
dTopK dict count fc =
  aggrFromFold
    (static (\Dict c f ->
      F.foldMap
        (\a -> TopK c . H.singleton $ H.Entry (f a) a)
        (\(TopK _ h) -> map H.payload . sortOn Down $ H.toUnsortedList h)
      ) `cap` dict `cap` cpure (static Dict) count `cap` fc
    )
    (static (\Dict c f ->
                F.Fold (\a b -> take c $ merge f a b) [] id
     ) `cap` dict `cap` cpure (static Dict) count `cap` fc
    )
  where
    merge _ xs [] = xs
    merge _ [] ys = ys
    merge f xss@(x:xs) yss@(y:ys) =
      if f x > f y
        then x:merge f xs yss
        else y:merge f xss ys

dBottomK :: (StaticSerialise a, Typeable k)
         => Closure (Dict (Ord k))
         -> Int              -- ^ Number of rows to return
         -> Closure (a -> k) -- ^ Sorting key
         -> Aggr a [a]
dBottomK d count fc =
  dTopK
    (static (\Dict -> Dict) `cap` d)
    count
    (static (Down .) `cap` fc)
