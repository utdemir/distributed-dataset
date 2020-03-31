{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Distributed.Dataset.Internal.Arrow where

import Codec.Serialise
import Conduit
import Control.Arrow
import Control.Category
import Control.Distributed.Closure
import Control.Distributed.Dataset.Internal.Class
import Data.Hashable

newtype Partition a = Partition [ConduitT () a (ResourceT IO) ()]
  deriving newtype (Semigroup, Monoid)

data Dataset a where
  DExternal ::
    [Partition a] ->
    Dataset a
  DPipe ::
    ConduitT a b (ResourceT IO) () ->
    Dataset a ->
    Dataset b
  DPartition ::
    (Hashable k, Serialise a) =>
    Int ->
    Closure (a -> k) ->
    Dataset a ->
    Dataset a

instance Functor Dataset where
  fmap f ds = DPipe (mapC f) ds

instance Applicative Dataset where
  pure a = DExternal [Partition [yield a]]
  l <*> r = undefined
