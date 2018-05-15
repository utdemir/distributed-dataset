{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

-- |
-- This module downloads every public GitHub event for the first quarter of 2018
-- from gharchive.com, and finds the people who used the word "cabal" in their
-- commit messages the most.
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
import           Data.List.Split                    (chunksOf)
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

artifactBucket :: T.Text
artifactBucket = "my-s3-bucket"

-- In gharchive, data is stored as gzipped JSON files for every hour since 2012.
--
-- Here we're iterating through every hour in 2017 and creating the urls to
-- process.
allUrls :: [String]
allUrls = do
  let (start, end) = ( fromGregorian 2018 1 1
                     , fromGregorian 2018 3 31
                     )
  date <- showGregorian <$> [start..end]
  time <- show <$> [(0::Int)..23]
  return $ "http://data.gharchive.org/" ++ date ++ "-" ++ time ++ ".json.gz"

-- Entrance point.
main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend -> do
    -- AWS Lambda starts with 3000 parallel tasks and slowly scales up with
    -- the rate of 500 per minute. So, for small tasks like this, we get the
    -- maximum throughput if we have less than 3000 executors since we don't
    -- have to wait for Lambda upscaling.
    -- See: https://docs.aws.amazon.com/lambda/latest/dg/scaling.html
    let chunkSize =
          min 8 . ceiling $ fromIntegral (length allUrls) / (3000 :: Double)
        chunks = chunksOf chunkSize allUrls

    -- Map every chunk to remote executors.
    results <- mapWithProgress backend (static Dict) $
      map
        (\xs -> static (mapM processUrl) `cap` cpure (static Dict) xs)
        chunks

    -- Combine results from diffent executors.
    let result = results
                   & concat
                   & MM.getMonoidalMap . foldMap MM.MonoidalMap

    -- Take the biggest 50 and print.
    result
      & take 20 . reverse . sortOn snd . M.toList
      & mapM_ (\(name, Sum count) ->
          putStrLn $ printf "%20s - %d" name count)

-- We actually don't need this much memory, since CPU increases linearly with
-- requested memory in AWS Lambda, more memory gives much better results, it
-- is even cheaper because functions finish more quickly.
opts :: LambdaBackendOptions
opts = lambdaBackendOptions artifactBucket
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
