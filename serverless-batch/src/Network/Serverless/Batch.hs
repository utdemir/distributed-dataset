{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Serverless.Batch where

--------------------------------------------------------------------------------
import GHC.Generics
import Data.Typeable
import Control.Distributed.Closure
import qualified Data.ByteString as BS
import Pipes
--------------------------------------------------------------------------------

data Dataset a where
  Read :: Typeable a => Closure Source -> Dataset a
  Map :: (Typeable a, Typeable b) => Closure (a -> b) -> Dataset a -> Dataset b
  Filter :: Closure (a -> Bool) -> Dataset a -> Dataset a
  Join
    :: Closure (a -> t)
    -> Closure (b -> t)
    -> Dataset a
    -> Dataset b
    -> Dataset (t, a, b)
  AggrByKey
    :: (Typeable a, Typeable b, Typeable k)
    => Aggr a b
    -> Closure (a -> k)
    -> Dataset a
    -> Dataset (k, b)

instance Show (Dataset a) where
  show (Map c a) = show a ++ " -> Map[" ++ show (typeRep c) ++ "]"
  show (Filter _ a) = show a ++ " -> Filter"
  show (Join _ _ a b) = "(" ++ show a ++ ", " ++ show b ++ ") -> Join"
  show (AggrByKey aggr extr a)
    = show a ++ " -> Aggr" ++ "[" ++ show (typeOf aggr) ++ ", " ++ show (typeRep extr) ++ "]"
  show (Read _) = "Read[" ++ show (typeRep (Proxy @a)) ++ "]"

data Action where
  Write :: Sink -> Dataset a -> Action

instance Show Action where
  show (Write _ a) = show a ++ " -> Write"

--------------------------------------------------------------------------------

newtype Seek = Seek Integer
  deriving (Show, Eq, Ord, Generic)

data Source = Source
  { sourceProducer :: Seek -> Producer BS.ByteString IO ()
  , sourceSize :: IO Integer
  }

data Sink = Sink
  { sinkConsumer :: Consumer BS.ByteString IO ()
  }

--------------------------------------------------------------------------------

data Aggr a b =
  forall t. Aggr (Closure (a -> t, t -> t -> t, t -> b))
