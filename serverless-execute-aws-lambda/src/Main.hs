{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Network.HTTP.Simple
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

opts :: LambdaBackendOptions
opts = LambdaBackendOptions { _lboBucket = "serverless-batch"
                            , _lboPrefix = "testprefix"
                            , _lboStackPrefix = "serverlessbatchtest"
                            }

main :: IO ()
main = do
  serverlessGuard
  withLambdaBackend opts $ \backend ->
    forConcurrently_ ([1 .. 100] :: [Int]) $ \i -> do
      ip <- execute backend (static Dict) (static whatismyip)
      putStrLn $ "lambda " ++ show i ++ ": " ++ T.unpack ip

whatismyip :: IO T.Text
whatismyip = T.decodeUtf8 . getResponseBody <$> httpBS "http://api.ipify.org"
