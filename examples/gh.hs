{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

--------------------------------------------------------------------------------
import Control.Monad (guard)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Zlib (ungzip)
import Network.HTTP.Simple (parseRequest, httpSource, getResponseBody)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Monoid (mconcat, Sum (Sum))
import Control.Lens ((^?), (&))
import Data.Time.Calendar (Day, showGregorian, fromGregorian)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
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
  withLambdaBackend opts $ \backend -> do
    asyncs <- mapM
      (\u -> threadDelay 10000 >> async (spawn backend u))
      (chunksOf 5 allUrls)
    result <- mconcat . mconcat <$> mapM wait asyncs
    result
      & M.toList . MM.getMonoidalMap
      & reverse . sortOn snd
      & mapM_ print . take 50

spawn :: Backend -> [String] -> IO [MM.MonoidalMap T.Text (Sum Int)]
spawn backend urls = do
  putStrLn $ "Spawning function for: " ++ show urls
  r <- execute
    backend
    (static Dict)
    (static (mapM processUrl) `cap` cpure (static Dict) urls)
  putStrLn $ "Got response from: " ++ show urls
  return $ MM.MonoidalMap <$> r


allUrls :: [String]
allUrls =
  [ "http://data.gharchive.org/" ++ d ++ "-" ++ t ++ ".json.gz"
  | d <- showGregorian <$> [startDate .. endDate]
  , t <- show <$> [(0::Int)..23]
  ]

startDate :: Day
startDate = fromGregorian 2017 12 1

endDate :: Day
endDate = fromGregorian 2017 31 31

processUrl :: String -> IO (M.Map T.Text (Sum Int))
processUrl str = do
  req <- parseRequest str
  res <- runConduitRes $
    httpSource req getResponseBody
      .| ungzip
      .| NDJ.valueParser
      .| C.foldMap processObject
  return $ MM.getMonoidalMap res

processObject :: Value -> MM.MonoidalMap T.Text (Sum Int)
processObject v = fromMaybe mempty $ do
  name <- v ^? key "actor" . key "login" . _String
  commits <- V.toList <$> v ^? key "payload" . key "commits" . _Array
  let msgs = catMaybes $ map (^? key "message" . _String) commits
      count = length $ filter (T.isInfixOf "monad" . T.toLower) msgs
  guard $ count > 0
  return $ MM.singleton name (Sum count)
