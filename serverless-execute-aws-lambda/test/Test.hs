{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StaticPointers #-}

module Main where

--------------------------------------------------------------------------------
import Data.Functor
import Network.AWS (newEnv, Credentials(Discover))
import Network.AWS.Auth (AuthError)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

main :: IO ()
main = do
  serverlessGuard
  aws <- hasAWSEnv
  if aws
    then defaultMain tests
    else putStrLn "AWS credentials not found, skipping tests."

hasAWSEnv :: IO Bool
hasAWSEnv =
  (newEnv Discover $> True) `catch` (\(_ :: AuthError) -> return False)

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
