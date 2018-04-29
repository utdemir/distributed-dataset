{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

--------------------------------------------------------------------------------
import Control.Concurrent.Async
import qualified Data.Map as M
import Data.Conduit
import Data.Conduit.BZlib
import Network.HTTP.Simple
import qualified Data.Conduit.JSON.NewlineDelimited as NDJ
import qualified Data.Conduit.Combinators as C
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
  initServerless
  withLambdaBackend opts $ \backend ->
    undefined

processUrl :: String -> IO (M.Map String Int)
processUrl str = do
  req <- parseRequest str
  runConduitRes $
    httpSource req getResponseBody
      .| bunzip2
      .| NDJ.valueParser
      .| C.foldMap undefined
