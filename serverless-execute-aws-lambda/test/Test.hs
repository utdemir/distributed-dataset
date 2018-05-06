{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception                 (try)
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as T
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
  tests_ <- lookupEnv "ENABLE_AWS_TESTS" >>= \case
    Nothing -> do
      putStrLn "ENV[ENABLE_AWS_TESTS] is not set, skipping."
      return $ testGroup "No tests" []
    Just _ -> return tests
  defaultMain tests_

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
    , testCase "handles oom" $ do
        r <-
          try $ withLambdaBackend opts $ \backend ->
            execute backend (static Dict) (static (return $ T.replicate (2^32) "h"))
        r @?= Left (ExecutorFailedException $
                      "Backend threw an exception: InvokeException " <>
                      "\"Lambda function failed.\"")
    ]
