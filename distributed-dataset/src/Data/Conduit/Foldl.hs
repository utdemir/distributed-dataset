{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Foldl where

import           Conduit
import qualified Control.Foldl as F

foldConduit :: forall a b m. Monad m => F.Fold a b -> ConduitT a Void m b
foldConduit (F.Fold step init extract) = go init
 where
  go acc = await >>= \case
    Nothing -> return $ extract acc
    Just a  -> go $ step acc a
