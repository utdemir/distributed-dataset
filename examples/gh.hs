{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

-- |
-- This module downloads every public GitHub event for the first half of 2018
-- from gharchive.com, and finds the people who used the word "cabal" the most
-- in their commit messages.
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
import           Data.Maybe                         (mapMaybe, fromMaybe)
import           Data.Monoid                        (Sum (Sum))
import qualified Data.Text                          as T
import           Data.Time.Calendar                 (fromGregorian,
                                                     showGregorian)
import qualified Data.Vector                        as V
import           Network.HTTP.Simple                (getResponseBody,
                                                     httpSource, parseRequest)
import           Text.Printf                        (printf)
--------------------------------------------------------------------------------
import           Control.Distributed.Fork
import           Control.Distributed.Fork.Lambda
import           Control.Distributed.Fork.Utils
--------------------------------------------------------------------------------

artifactBucket :: T.Text
artifactBucket = "my-s3-bucket"

-- In gharchive, data is stored as gzipped JSON hourly.
--
-- Here we're iterating through every hour in first half of 2018 and creating
-- urls to process. This results in a 76.4 GB download.
allUrls :: [String]
allUrls = do
  let (start, end) = ( fromGregorian 2018 1 1
                     , fromGregorian 2018 6 1
                     )
  date <- showGregorian <$> [start..end]
  time <- show <$> [(0::Int)..23]
  return $ "http://data.gharchive.org/" ++ date ++ "-" ++ time ++ ".json.gz"

main :: IO ()
main = do
  initDistributedFork
  withLambdaBackend opts $ \backend -> do
    -- Here is where the magic happens. Map urls to remote executors, run them
    -- and fetch the results.
    results <- mapConcurrentlyWithProgress backend (static Dict) $
      map
        (\xs -> static processUrl `cap` cpure (static Dict) xs)
        allUrls

    -- Combine results from diffent executors.
    let result = MM.getMonoidalMap $ foldMap MM.MonoidalMap results

    -- Take the biggest 50 and print.
    result
      & take 20 . reverse . sortOn snd . M.toList
      & mapM_ (\(name, Sum count) ->
          putStrLn $ printf "%20s - %d" name count)

-- We actually don't need this much memory. But since CPU allocation increases
-- linearly with requested memory in AWS Lambda, more memory gives much faster
-- results; it is even cheaper because functions finish more quickly.
opts :: LambdaBackendOptions
opts = lambdaBackendOptions artifactBucket
         & lboMemory .~ 1024

--------------------------------------------------------------------------------

-- Here, we make the actual HTTP request; extract, parse and gather the results
-- in a streaming fashion.
processUrl :: String -> IO (M.Map T.Text (Sum Int))
processUrl url = do
  req <- parseRequest url
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
  let msgs = mapMaybe (^? key "message" . _String) commits
      count = length $ filter (T.isInfixOf "cabal") msgs
  guard $ count > 0
  return $ MM.singleton name (Sum count)
