{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Closure              (unclosure)
import           Control.Exception                        (try)
import           Control.Monad                            (unless)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Conduit                             hiding (await)
import qualified Data.Conduit.Combinators                 as C
import qualified Data.Text                                as T
import           Hedgehog
import           Language.Haskell.TH                      (unType)
import           System.Exit                              (exitFailure)
--------------------------------------------------------------------------------
import           Control.Distributed.Dataset.AWS
import           Control.Distributed.Dataset.ShuffleStore
import           Control.Distributed.Fork
--------------------------------------------------------------------------------

prop_backendCreate :: Property
prop_backendCreate = property $
  liftIO . withLambdaBackend opts $ \_ -> do
    threadDelay $ 5*1000*1000
    return ()

prop_backendFork :: Property
prop_backendFork = property $ do
  r <-
    liftIO . withLambdaBackend opts $ \backend ->
      fork backend (static Dict) (static (return $ Just @Integer 42))
        >>= await
  r === Just 42

prop_backendLargeReturnValue :: Property
prop_backendLargeReturnValue = property $ do
  r <-
    liftIO . withLambdaBackend opts $ \backend ->
      fork backend (static Dict) (static (return $ T.replicate (pow 2 22) "h"))
        >>= await
  T.length r === pow 2 22

prop_backendOOM :: Property
prop_backendOOM = property $ do
  r <-
    liftIO . try $ withLambdaBackend opts $ \backend ->
      fork backend (static Dict) (static (return $ T.replicate (pow 2 32) "h"))
        >>= await
  case r of
    Left (ExecutorFailedException t)
      | "Backend threw an exception: InvokeException \"Lambda function failed"
        `T.isPrefixOf` t -> return ()
    Left other -> do
      annotateShow other
      failure
    Right _ ->
      failure

prop_shuffleStorePut :: Property
prop_shuffleStorePut = property $ do
  let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
  liftIO . runConduitRes $
    mapM_ yield ["foo", "bar", "baz"]
      .| (unclosure $ ssPut ss) 42

prop_shuffleStoreGetAfterPut :: Property
prop_shuffleStoreGetAfterPut = property $ do
  let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
  liftIO . runConduitRes $
    mapM_ yield ["foo", "bar", "baz"]
      .| (unclosure $ ssPut ss) 42
  r <- liftIO . runConduitRes $
        (unclosure $ ssGet ss) 42 RangeAll
          .| C.foldMap id
  r === "foobarbaz"

prop_shuffleStoreGetRangeAfterPut :: Property
prop_shuffleStoreGetRangeAfterPut = property $ do
  let ss = s3ShuffleStore "distributed-dataset" "shuffle-store/"
  liftIO . runConduitRes $
    mapM_ yield ["foo", "bar", "baz"]
      .| (unclosure $ ssPut ss) 42
  r <- liftIO . runConduitRes $
        (unclosure $ ssGet ss) 42 (RangeOnly 3 5)
          .| C.foldMap id
  r === "bar"

pow :: Int -> Int -> Int
pow = (^)

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "distributed-dataset"

main :: IO ()
main = do
  initDistributedFork
  result <- checkSequential $(unType <$> discover)
  unless result exitFailure

