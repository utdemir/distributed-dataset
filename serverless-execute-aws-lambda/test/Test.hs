{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StaticPointers #-}

module Main where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

main :: IO ()
main = do
  serverlessGuard
  defaultMain tests

opts :: LambdaBackendOptions
opts = LambdaBackendOptions { _lboBucket = "serverless-batch"
                            , _lboPrefix = "testprefix"
                            , _lboStackPrefix = "serverlessbatchtest"
                            }

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "creates backend" $ do withLambdaBackend opts $ const (return ())
    , testCase "runs the function" $ do
        r <-
          withLambdaBackend opts $ \backend ->
            execute backend (static Dict) (static (return $ Just @Integer 42))
        print r
        r @?= Just 42
  ]
