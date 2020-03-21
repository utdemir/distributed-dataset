{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Closure (unclosure)
import Control.Distributed.Dataset
import Control.Distributed.Fork.SSH
import Control.Monad
import Control.Monad.IO.Class
import Hedgehog
import Language.Haskell.TH (unType)
import System.Exit (exitFailure)

main :: IO ()
main = do
  initDistributedFork
  spawnSSH
