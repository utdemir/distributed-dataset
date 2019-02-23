module Data.Monoid.Commutative
  ( CommutativeSemigroup
  , CommutativeMonoid
  , module Data.Monoid
  ) where

import           Data.Monoid

class Semigroup m => CommutativeSemigroup m
class (CommutativeSemigroup m, Monoid m) => CommutativeMonoid m

instance CommutativeMonoid ()
instance CommutativeSemigroup ()

instance (CommutativeSemigroup a, CommutativeSemigroup b) => CommutativeSemigroup (a, b)
instance (CommutativeMonoid a, CommutativeMonoid b) => CommutativeMonoid (a, b)

instance Num a => CommutativeSemigroup (Sum a)
instance Num a => CommutativeMonoid (Sum a)
