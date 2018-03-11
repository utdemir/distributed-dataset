{-# LANGUAGE StaticPointers #-}

module Main where

--------------------------------------------------------------------------------
import System.Posix.Process
import Test.Tasty
import Test.Tasty.HUnit
--------------------------------------------------------------------------------
import Network.Serverless.Execute
--------------------------------------------------------------------------------

main :: IO ()
main = do
  serverlessGuard
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
            (static (return (42 :: Integer, "fortytwo", Just True)))
        ret @?= (42, "fortytwo", Just True)
    , testCase "localProcessBackend actually spawns" $ do
        localPid <- show <$> getProcessID
        remotePid <-
          execute
            localProcessBackend
            (static Dict)
            (static (show <$> getProcessID))
        remotePid /= localPid @? "Spawned process should have a different PID."
    ]
