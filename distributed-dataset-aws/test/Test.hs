{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Exception               (try)
import           Data.Conduit hiding (await)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                       as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Distributed.Closure (unclosure)
--------------------------------------------------------------------------------
import           Control.Distributed.Fork
import           Control.Distributed.Dataset.AWS
import           Control.Distributed.Dataset.ShuffleStore
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initDistributedFork
  defaultMain tests

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "distributed-dataset"

tests :: TestTree
tests =
  testGroup "distributed-dataset-aws"
    [ testGroup "Backend"
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
              fork backend (static Dict) (static (return $ T.replicate (pow 2 22) "h"))
                >>= await
          T.length r @?= pow 2 22
      , testCase "handles oom" $ do
          r <-
            try $ withLambdaBackend opts $ \backend ->
              fork backend (static Dict) (static (return $ T.replicate (pow 2 32) "h"))
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
    , testGroup "ShuffleStore" $
      [ testCase "put" $ do
          let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
          runConduitRes $
            mapM_ yield ["foo", "bar", "baz"]
              .| (unclosure $ ssPut ss) 42
      , testCase "get after put" $ do
          let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
          runConduitRes $
            mapM_ yield ["foo", "bar", "baz"]
              .| (unclosure $ ssPut ss) 42
          r <- runConduitRes $
                (unclosure $ ssGet ss) 42 RangeAll
                  .| C.foldMap id
          r @?= "foobarbaz"
      , testCase "get range after put" $ do
          let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
          runConduitRes $
            mapM_ yield ["foo", "bar", "baz"]
              .| (unclosure $ ssPut ss) 42
          r <- runConduitRes $
                (unclosure $ ssGet ss) 42 (RangeOnly 3 5)
                  .| C.foldMap id
          r @?= "bar"
      ]
    ]
  where
    pow :: Int -> Int -> Int
    pow = (^)
