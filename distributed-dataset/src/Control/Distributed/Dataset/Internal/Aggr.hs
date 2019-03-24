{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TypeApplications           #-}

module Control.Distributed.Dataset.Internal.Aggr
  ( Aggr (..)
  , dConstAggr
  , dCount
  , dSum
  , dAvg
  , dCollect
  , dBottomK
  , dTopK
  ) where

-------------------------------------------------------------------------------
import           Control.Applicative.Static
import           Control.Distributed.Closure
import qualified Control.Foldl                              as F
import           Data.Functor.Static
import qualified Data.Heap                                  as H
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Profunctor
import           Data.Profunctor.Static
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Class
-------------------------------------------------------------------------------

-- |
-- Composable aggregations.
--
-- You can use the 'StaticApplicative' and 'StaticProfunctor' instances to
-- compose multiple 'Aggr's together. See the implementation of 'dAvg' function
-- for an example.
data Aggr a b =
  forall t. StaticSerialise t =>
  Aggr
    (Closure (F.Fold a t))
    (Closure (F.Fold t b))

instance Typeable m => StaticFunctor (Aggr m) where
  staticMap f (Aggr f1c f2c)
    = Aggr f1c (static fmap `cap` f `cap` f2c)

instance Typeable m => StaticApply (Aggr m) where
  staticApply (Aggr f1c f2c) (Aggr f1c' f2c') =
    Aggr (static (\f1 f1' -> (,) <$> f1 <*> f1') `cap` f1c `cap` f1c')
         (static (\f2 f2' -> ($) <$> lmap fst f2 <*> lmap snd f2') `cap` f2c `cap` f2c')

instance StaticProfunctor Aggr where
  staticDimap l r (Aggr f1 f2) =
    Aggr (static lmap `cap` l `cap` f1)
         (static rmap `cap` r `cap` f2)

aggrFromMonoid :: StaticSerialise a
               => Closure (Dict (Monoid a))
               -> Aggr a a
aggrFromMonoid d
  = aggrFromFold go go
 where
  go = static (\Dict -> F.foldMap id id) `cap` d

aggrFromFold :: StaticSerialise t
             => Closure (F.Fold a t)
             -> Closure (F.Fold t b)
             -> Aggr a b
aggrFromFold = Aggr


-- |
-- An aggregation which ignores the input data and always yields the given value.
--
-- Useful with 'staticApply'. e.g:
--
-- @
-- dAvg :: Aggr Double Double
-- dAvg =
--   dConstAggr (static (/))
--     `staticApply` dSum (static Dict)
--     `staticApply` staticMap (static realToFrac) dCount
-- @
dConstAggr :: forall a t. (Typeable a, Typeable t) => Closure a -> Aggr t a
dConstAggr ac =
  staticDimap
    (static (const ()))
    (static const `cap` ac)
    (aggrFromMonoid (static Dict))

dSum :: StaticSerialise a => Closure (Dict (Num a)) -> Aggr a a
dSum d =
  staticDimap
    (static Sum)
    (static getSum)
    (aggrFromMonoid $ static (\Dict -> Dict) `cap` d)

dCount :: Typeable a => Aggr a Integer
dCount =
  static (const 1) `staticLmap` dSum (static Dict)

dAvg :: Aggr Double Double
dAvg =
  dConstAggr (static (/))
    `staticApply` dSum (static Dict)
    `staticApply` staticMap (static realToFrac) dCount

-- * Collect

-- |
-- Warning: Ordering of the resulting list is non-deterministic.
dCollect :: StaticSerialise a => Aggr a [a]
dCollect =
  aggrFromFold
    (static F.list)
    (static (concat <$> F.list))

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
      -> Int
      -> Closure (a -> k)
      -> Aggr a [a]
dTopK dict count fc =
  aggrFromFold
    ((static (\Dict c f ->
      F.foldMap
        (\a -> TopK c . H.singleton $ H.Entry (f a) a)
        (\(TopK _ h) -> map H.payload . sortBy (comparing Down) $ H.toUnsortedList h)
      )
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
         -> Int
         -> Closure (a -> k)
         -> Aggr a [a]
dBottomK d count fc =
  dTopK
    (static (\Dict -> Dict) `cap` d)
    count
    (static (Down .) `cap` fc)
