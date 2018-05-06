{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import Control.Exception (try)
import           System.Posix.Process
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.LocalProcessBackend
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initServerless
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "localProcessBackend gives correct result" $ do
        ret <-
          execute
            localProcessBackend
            (static Dict)
            (static (return (42 :: Integer, "fortytwo" :: String, Just True)))
        ret @?= (42, "fortytwo", Just True)
    , testCase "localProcessBackend actually spawns" $ do
        localPid <- show <$> getProcessID
        remotePid <-
          execute
            localProcessBackend
            (static Dict)
            (static (show <$> getProcessID))
        remotePid /= localPid @? "Spawned process should have a different PID."
    , testCase "localProcessBackend handles exceptions" $ do
        answer <-
          try $ execute
            localProcessBackend
            (static Dict)
            (static (return (div 1 0) :: IO Int))
        answer @?= Left (ExecutorFailedException "Exception from executor: divide by zero")
    ]
