{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Exception                              (try)
import           System.Posix.Process                           (getProcessID)
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Control.Distributed.Fork
import           Control.Distributed.Fork.LocalProcessBackend
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initDistributedFork
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "localProcessBackend gives correct result" $ do
        ret <-
          await =<< fork
            localProcessBackend
            (static Dict)
            (static (return (42 :: Integer, "fortytwo" :: String, Just True)))
        ret @?= (42, "fortytwo", Just True)
    , testCase "localProcessBackend actually spawns" $ do
        localPid <- show <$> getProcessID
        remotePid <-
          await =<< fork
            localProcessBackend
            (static Dict)
            (static (show <$> getProcessID))
        remotePid /= localPid @? "Spawned process should have a different PID."
    , testCase "localProcessBackend handles exceptions" $ do
        handle <- fork
          localProcessBackend
          (static Dict)
          (static (return (div 1 0) :: IO Int))
        answer <- try $ await handle
        answer @?= Left (ExecutorFailedException "Exception from executor: divide by zero")
    ]
