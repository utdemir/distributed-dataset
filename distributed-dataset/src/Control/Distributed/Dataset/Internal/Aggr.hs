{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StaticPointers            #-}
{-# LANGUAGE TypeApplications          #-}

module Control.Distributed.Dataset.Internal.Aggr where

-------------------------------------------------------------------------------
import           Control.Applicative.Static
import           Control.Distributed.Closure
import           Data.Functor.Static
import           Data.Monoid.Commutative
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
  Aggr (Closure (a -> t))                     -- ^ Map
       (Closure (Dict (CommutativeMonoid t))) -- ^ Reduce
       (Closure (t -> b))                     -- ^ Extract

instance Typeable m => StaticFunctor (Aggr m) where
  staticMap f (Aggr m r e) = Aggr m r (static (.) `cap` f `cap` e)

instance Typeable m => StaticApply (Aggr m) where
  staticApply (Aggr m1 r1 e1) (Aggr m2 r2 e2) =
    Aggr (static (\m1' m2' a -> (m1' a, m2' a)) `cap` m1 `cap` m2)
         (static (\Dict Dict -> Dict) `cap` r1 `cap` r2)
         (static (\e1' e2' (b1, b2) -> (e1' b1) (e2' b2)) `cap` e1 `cap` e2)

instance StaticProfunctor Aggr where
  staticDimap l r (Aggr m d e) =
    Aggr (static (.) `cap` m `cap` l)
         d
         (static (.) `cap` r `cap` e)

-- |
-- An aggregation which ignores the input data and always yields the given value.
dConstAggr :: forall a t. (Typeable a, Typeable t) => Closure a -> Aggr t a
dConstAggr ca =
  Aggr (static (const ()))
       (static Dict)
       (static const `cap` ca)

dSum :: StaticSerialise a => Closure (Dict (Num a)) -> Aggr a a
dSum d =
  Aggr (static (\Dict -> Sum) `cap` d)
       (static (\Dict -> Dict) `cap` d)
       (static getSum)

dCount :: Typeable a => Aggr a Integer
dCount =
  static (const 1) `staticLmap` dSum (static Dict)

dAvg :: Aggr Double Double
dAvg =
  dConstAggr (static (/))
    `staticApply` dSum (static Dict)
    `staticApply` staticMap (static realToFrac) dCount

