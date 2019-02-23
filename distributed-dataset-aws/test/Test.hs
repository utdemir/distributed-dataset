{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception               (try)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Control.Distributed.Fork
import           Control.Distributed.Fork.Lambda
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initDistributedFork
  defaultMain tests

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-dataset"

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
            fork backend (static Dict) (static (return $ Just @Integer 42))
              >>= await
        r @?= Just 42
    , testCase "handles large return value" $ do
        r <-
          withLambdaBackend opts $ \backend ->
            fork backend (static Dict) (static (return $ T.replicate (2^22) "h"))
              >>= await
        T.length r @?= (2 ^ 22)
    , testCase "handles oom" $ do
        r <-
          try $ withLambdaBackend opts $ \backend ->
            fork backend (static Dict) (static (return $ T.replicate (2^32) "h"))
              >>= await
        case r of
          Left (ExecutorFailedException t)
            | "Backend threw an exception: InvokeException \"Lambda function failed"
              `T.isPrefixOf` t -> return ()
          Left other ->
            assertFailure $ "invalid exception: " ++ show other
          Right _ ->
            assertFailure "should have failed."
    ]
