{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

--------------------------------------------------------------------------------
import           Test.Tasty
--------------------------------------------------------------------------------
import           BatchTests
import           Control.Distributed.Fork (initDistributedFork)
import           SerialiseTests
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initDistributedFork
  defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests"
  [ serialiseTests
  , datasetTests
  ]
