{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Serverless.Batch where

--------------------------------------------------------------------------------
import Pipes
import Data.Typeable
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
    :: Closure (Dict (Typeable t))
    -> Closure (a -> t)
    -> Closure (b -> t)
    -> Dataset a
    -> Dataset b
    -> Dataset (t, a, b)

instance Show (Dataset a) where
  show (DRead _ _) = "Read[" ++ show (typeRep (Proxy @a)) ++ "]"
  show (DBoundary a) = show a ++ "-> Boundary"
  show (DMap c a) = show a ++ " -> Map[" ++ show (typeRep c) ++ "]"
  show (DFilter _ a) = show a ++ " -> Filter"
  show (DJoin _ _ _ a b) = "(" ++ show a ++ ", " ++ show b ++ ") -> Join"

data Action where
  Write :: Closure Sink -> FilePath -> Dataset a -> Action

instance Show Action where
  show (Write _ _ a) = show a ++ " -> Write"

runAction :: Backend -> Action -> IO ()
runAction _ = print

--------------------------------------------------------------------------------

data SimplifiedDataset a where
  SDRead :: Source -> FilePath -> SimplifiedDataset a
  SDBoundary :: SimplifiedDataset a -> SimplifiedDataset a
  SDNarrow
    :: (Typeable a, Typeable b)
    => Closure (Pipe a b IO ())
    -> SimplifiedDataset a
    -> SimplifiedDataset b
  SDWide
    :: (Typeable a, Typeable b)
    => Closure (Pipe a (Int, b) IO ())
    -> SimplifiedDataset a
    -> SimplifiedDataset b
  SDReadCorresponding
    :: SimplifiedDataset a
    -> SimplifiedDataset b
    -> Closure (Pipe (Either a b) c IO ())
    -> SimplifiedDataset c

simplify :: Dataset a -> SimplifiedDataset a
simplify (DRead s p)
  = SDRead s p
simplify (DBoundary d)
  = SDBoundary (simplify d)
simplify (DMap f d)
  = SDNarrow (static P.map `cap` f) (simplify d)
simplify (DFilter f d)
  = SDNarrow (static P.filter `cap` f) (simplify d)
simplify (DJoin d ka kb da db) = undefined
--  case unclosure d of
--    Dict ->
--      SDReadCorresponding
--      (SDWide (static (\ka -> P.map (\a -> (partition (ka a), a))) `cap` ka) (simplify da))
--      (undefined)
--      (undefined)
--  where
--    partition :: k -> Int
--    partition = undefined
