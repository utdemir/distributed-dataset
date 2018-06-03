{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

--------------------------------------------------------------------------------
import           Control.Concurrent.Async          (forConcurrently_)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Network.HTTP.Simple               (getResponseBody, httpBS)
--------------------------------------------------------------------------------
import           Control.Distributed.Fork
import           Control.Distributed.Fork.Lambda
--------------------------------------------------------------------------------

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-batch"

main :: IO ()
main = do
  initDistributedFork
  withLambdaBackend opts $ \backend ->
    forConcurrently_ [1 .. 32] $ \i -> do
      putStrLn $ "Invoking function " ++ show (i :: Int)
      ip <- await =<< fork backend (static Dict) (static whatismyip)
      putStrLn $ "Returned " ++ show i ++ ": " ++ T.unpack ip

whatismyip :: IO T.Text
whatismyip = T.decodeUtf8 . getResponseBody <$> httpBS "http://api.ipify.org"
