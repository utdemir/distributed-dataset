module Main where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import BatchTests
import Control.Distributed.Fork (initDistributedFork)
import Control.Monad
import Hedgehog
import SerialiseTests
import System.Exit

--------------------------------------------------------------------------------
main :: IO ()
main = do
  initDistributedFork
  result <- and <$> mapM checkSequential [batchTests, serialiseTests]
  unless result exitFailure
