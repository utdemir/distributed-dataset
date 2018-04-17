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
import Data.List (sort)
import Data.Typeable
import Data.These
import Data.Constraint
import Control.Monad
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
    -> Closure (Dict (Ord t))
    -> Closure (a -> t)
    -> Closure (b -> t)
    -> Dataset a
    -> Dataset b
    -> Dataset (These a b)

instance Show (Dataset a) where
  show (DRead _ _) = "Read[" ++ show (typeRep (Proxy @a)) ++ "]"
  show (DBoundary a) = show a ++ "-> Boundary"
  show (DMap c a) = show a ++ " -> Map[" ++ show (typeRep c) ++ "]"
  show (DFilter _ a) = show a ++ " -> Filter"
  show (DJoin _ _ _ _ _ _ _ a b) = "(" ++ show a ++ ", " ++ show b ++ ") -> Join"

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
    -> Closure (Pipe (Maybe a) (Int, b) IO ())
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
simplify (DJoin dta dtb dtk dhk dok (ka :: Closure (a -> k)) (kb :: Closure (b -> k)) dsa dsb) =
  case (unclosure dta, unclosure dtb, unclosure dtk) of
    (Dict, Dict, Dict) ->
      SDReadCorresponding
      (SDWide
        dta
        dta
        undefined -- (static (\dhk dok ka -> pSort @a dok >-> pPartitionBy @a @k dhk ka) `cap` dhk `cap` dok `cap` ka)
        (simplify dsa))
      (SDWide
        dtb
        dtb
        undefined -- (static (\dhk dok kb -> pSort @b dok >-> pPartitionBy @b @k dhk kb) `cap` dhk `cap` dok `cap` kb)
        (simplify dsb))
      (static (pMerge @a @b) `cap` ka `cap` kb)

pPartitionBy ::
     forall a b. Dict (Hashable b) -> (a -> b) -> Pipe a (Int, a) IO ()
pPartitionBy Dict f = P.map $ \a -> (partition (f a), a)
  where
    partition :: Hashable j => j -> Int
    partition t = hash t `mod` 200

pMerge :: forall a b t. (a -> t) -> (b -> t) -> Pipe (Either (Maybe a) (Maybe b)) (These a b) IO ()
pMerge = undefined

pSort :: forall a. Dict (Ord a) -> Pipe (Maybe a) a IO ()
pSort Dict = go []
  where
    go xs =
      await >>= \case
        Nothing -> forM_ (sort xs) yield
        Just x -> go (x : xs)
