{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

--------------------------------------------------------------------------------
import           Control.Concurrent.Async          (forConcurrently_)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Network.HTTP.Simple               (getResponseBody, httpBS)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "my-s3-bucket"

main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend ->
    forConcurrently_ [1 .. 16] $ \i -> do
      putStrLn $ "Invoking function " ++ show (i :: Int)
      ip <- execute backend (static Dict) (static whatismyip)
      putStrLn $ "Returned " ++ show i ++ ": " ++ T.unpack ip

whatismyip :: IO T.Text
whatismyip = T.decodeUtf8 . getResponseBody <$> httpBS "http://api.ipify.org"
