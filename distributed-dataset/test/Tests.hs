module Main where

--------------------------------------------------------------------------------
import           Control.Monad
import           Hedgehog
import           System.Exit
--------------------------------------------------------------------------------
import           BatchTests
import           Control.Distributed.Fork (initDistributedFork)
import           SerialiseTests
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initDistributedFork
  result <- and <$> mapM checkSequential [batchTests, serialiseTests]
  unless result exitFailure
