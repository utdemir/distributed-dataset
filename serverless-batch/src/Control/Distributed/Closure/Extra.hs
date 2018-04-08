{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Process.Extra
  ( module Control.Distributed.Closure
  ) where

--------------------------------------------------------------------------------
import Data.Binary
import Data.Typeable
import Control.Distributed.Closure
------------------------------------------------------------------------------

instance Typeable a => Eq (Closure a) where
  c1 == c2 = encode c1 == encode c2
