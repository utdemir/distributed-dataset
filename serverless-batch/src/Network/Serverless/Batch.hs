{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Serverless.Batch where

--------------------------------------------------------------------------------
import Pipes
import Data.Hashable
import Data.Typeable
import Data.Constraint
import Control.Distributed.Closure
import qualified Pipes.Prelude as P
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Batch.IO
--------------------------------------------------------------------------------

data Dataset a where
  DRead :: Typeable a => Source -> FilePath -> Dataset a
  DBoundary :: Dataset a -> Dataset a
  DMap :: (Typeable a, Typeable b) => Closure (a -> b) -> Dataset a -> Dataset b
  DFilter :: Typeable a => Closure (a -> Bool) -> Dataset a -> Dataset a
  DJoin
    :: Closure (Dict (Typeable a))
    -> Closure (Dict (Typeable b))
    -> Closure (Dict (Typeable t))
    -> Closure (Dict (Hashable t))
    -> Closure (a -> t)
    -> Closure (b -> t)
    -> Dataset a
    -> Dataset b
    -> Dataset (a, b)

instance Show (Dataset a) where
  show (DRead _ _) = "Read[" ++ show (typeRep (Proxy @a)) ++ "]"
  show (DBoundary a) = show a ++ "-> Boundary"
  show (DMap c a) = show a ++ " -> Map[" ++ show (typeRep c) ++ "]"
  show (DFilter _ a) = show a ++ " -> Filter"
  show (DJoin _ _ _ _ _ _ a b) = "(" ++ show a ++ ", " ++ show b ++ ") -> Join"

data Action where
  Write :: Closure Sink -> FilePath -> Dataset a -> Action

instance Show Action where
  show (Write _ _ a) = show a ++ " -> Write"

runAction :: Backend -> Action -> IO ()
runAction _ = print

--------------------------------------------------------------------------------

data SimpleDataset a where
  SDRead :: Source -> FilePath -> SimpleDataset a
  SDBoundary :: SimpleDataset a -> SimpleDataset a
  SDNarrow
    :: (Typeable a, Typeable b)
    => Closure (Pipe a b IO ())
    -> SimpleDataset a
    -> SimpleDataset b
  SDWide
    :: Closure (Dict (Typeable a))
    -> Closure (Dict (Typeable b))
    -> Closure (Pipe a (Int, b) IO ())
    -> SimpleDataset a
    -> SimpleDataset b
  SDReadCorresponding
    :: SimpleDataset a
    -> SimpleDataset b
    -> Closure (Pipe (Either (Maybe a) (Maybe b)) c IO ())
    -> SimpleDataset c

simplify :: Dataset a -> SimpleDataset a
simplify (DRead s p)
  = SDRead s p
simplify (DBoundary d)
  = SDBoundary (simplify d)
simplify (DMap f d)
  = SDNarrow (static P.map `cap` f) (simplify d)
simplify (DFilter f d)
  = SDNarrow (static P.filter `cap` f) (simplify d)
simplify (DJoin dta dtb dtk dhk (ka :: Closure (a -> k)) (kb :: Closure (b -> k)) dsa dsb) =
  case (unclosure dta, unclosure dtb, unclosure dtk) of
    (Dict, Dict, Dict) ->
      SDReadCorresponding
      (SDWide
        dta
        dta
        (static (partitioner @a @k) `cap` dhk `cap` ka)
        (simplify dsa))
      (SDWide
        dtb
        dtb
        (static (partitioner @b @k) `cap` dhk `cap` kb)
        (simplify dsb))
      (static (combine @a @b))
  where
    partition :: Hashable j => j -> Int
    partition t = hash t `mod` 200

    partitioner :: forall g j.
                   Dict (Hashable j)
                -> (g -> j)
                -> Pipe g (Int, g) IO ()
    partitioner Dict f = P.map $ \g -> (partition (f g), g)

    combine :: forall g j. Pipe (Either (Maybe g) (Maybe j)) (g, j) IO ()
    combine = undefined

