{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initServerless
  lookupEnv "ENABLE_AWS_TESTS" >>= \case
    Nothing -> putStrLn "ENV[ENABLE_AWS_TESTS] is not set, skipping."
    Just _ -> defaultMain tests

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-batch"

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "creates backend" $ do
        withLambdaBackend opts $ \_ -> do
          threadDelay $ 5*1000*1000
          return ()
    , testCase "runs the function" $ do
        r <-
          withLambdaBackend opts $ \backend ->
            execute backend (static Dict) (static (return $ Just @Integer 42))
        r @?= Just 42
  ]
