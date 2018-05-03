{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Data.Functor
import           Network.AWS                       (Credentials (Discover),
                                                    newEnv)
import           Network.AWS.Auth                  (AuthError)
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initServerless
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
