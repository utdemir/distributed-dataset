{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

-- |
-- This module downloads every public GitHub event for the last year from
-- gharchive.com, and finds the people who used the word "cabal" in their commit
-- messages the most.
module Main where

--------------------------------------------------------------------------------
import           Control.Lens                       ((&), (.~), (^?))
import           Control.Monad                      (guard)
import           Data.Aeson                         (Value)
import           Data.Aeson.Lens                    (key, _Array, _String)
import           Data.Conduit                       (runConduitRes, (.|))
import qualified Data.Conduit.Combinators           as C
import qualified Data.Conduit.JSON.NewlineDelimited as NDJ
import           Data.Conduit.Zlib                  (ungzip)
import           Data.List                          (sortOn)
import qualified Data.Map                           as M
import qualified Data.Map.Monoidal                  as MM
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Monoid                        (Sum (Sum))
import qualified Data.Text                          as T
import           Data.Time.Calendar                 (fromGregorian,
                                                     showGregorian)
import qualified Data.Vector                        as V
import           Network.HTTP.Simple                (getResponseBody,
                                                     httpSource, parseRequest)
import           Text.Printf                        (printf)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
import           Network.Serverless.Execute.Utils
--------------------------------------------------------------------------------

-- In gharchive, data is stored as gzipped JSON files for every hour since 2012.
--
-- Here we're iterating through every hour in 2017 and creating the urls to
-- process.
allUrls :: [String]
allUrls = do
  let (start, end) = ( fromGregorian 2018 1 1
                     , fromGregorian 2018 5 10
                     )
  date <- showGregorian <$> [start..end]
  time <- show <$> [(0::Int)..23]
  return $ "http://data.gharchive.org/" ++ date ++ "-" ++ time ++ ".json.gz"

-- Entrance point.
main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend -> do
    -- For every url, create a separate thread which spawns a separate Lambda
    -- function.
    results <- mapWithProgress backend (static Dict) $
      map (\u -> static processUrl `cap` cpure (static Dict) u) allUrls

    -- Combine results from diffent executors
    let result = MM.getMonoidalMap . foldMap MM.MonoidalMap $ results

    -- Take the biggest 50 and print.
    result
      & take 20 . reverse . sortOn snd . M.toList
      & mapM_ (\(name, Sum count) ->
          putStrLn $ printf "%20s - %d" name count)

-- We actually don't need this much memory, since CPU increases linearly with
-- requested memory in AWS Lambda, more memory gives much better results, it
-- is even cheaper because functions finish more quickly.
opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-batch"
         & lboMemory .~ 1024

--------------------------------------------------------------------------------

-- Here, we make the actual HTTP request; extract, parse and gather the results
-- in a streaming fashion.
processUrl :: String -> IO (M.Map T.Text (Sum Int))
processUrl str = do
  req <- parseRequest str
  res <- runConduitRes $
    httpSource req getResponseBody
      .| ungzip
      .| NDJ.valueParser
      .| C.foldMap processObject
  return $ MM.getMonoidalMap res

-- This part extracts commit messages and creates a Map for the users with the
-- number of commit messages containing "cabal".
processObject :: Value -> MM.MonoidalMap T.Text (Sum Int)
processObject v = fromMaybe mempty $ do
  name <- v ^? key "actor" . key "login" . _String
  commits <- V.toList <$> v ^? key "payload" . key "commits" . _Array
  let msgs = catMaybes $ map (^? key "message" . _String) commits
      count = length $ filter (T.isInfixOf "cabal" . T.toLower) msgs
  guard $ count > 0
  return $ MM.singleton name (Sum count)
