{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}

module Control.Distributed.Dataset.Internal.Aggr
  ( Aggr (..)
  , aggrFromMonoid
  , aggrFromFold
  , dConstAggr
  ) where

-------------------------------------------------------------------------------
import           Control.Applicative.Static
import           Control.Distributed.Closure
import qualified Control.Foldl                              as F
import           Data.Functor.Static
import           Data.Profunctor
import           Data.Profunctor.Static
import           Data.Typeable
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Class
-------------------------------------------------------------------------------

-- |
-- Represent an aggregation which takes many 'a's and returns a single 'b'.
--
-- Use 'Control.Distributed.Fork.dAggr' and 'Control.Distributed.Fork.dGroupedAggr'
-- functions to use them on 'Dataset's.
--
-- You can use the 'StaticApply' and 'StaticProfunctor' instances to compose
-- 'Aggr's together. See the implementation of 'dAvg' function for an example.
--
-- Alternatively, you can use 'aggrFromMonoid' and 'aggrFromFold' functions to
-- create 'Aggr's.
data Aggr a b =
  forall t. (StaticSerialise t, Typeable a, Typeable b) =>
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

aggrFromFold :: (StaticSerialise t, Typeable a, Typeable b)
             => Closure (F.Fold a t) -- ^ Fold to run before the shuffle
             -> Closure (F.Fold t b) -- ^ Fold to run after the shuffle
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
