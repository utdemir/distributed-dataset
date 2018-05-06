{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

-- |
-- This module downloads every public GitHub event for the last year from
-- gharchive.com, and finds the people who used the word "cabal" in their commit
-- messages the most.
module Main where

--------------------------------------------------------------------------------
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.Async           (async, wait)
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
import           Data.Monoid                        (Sum (Sum), mconcat)
import qualified Data.Text                          as T
import           Data.Time.Calendar                 (fromGregorian,
                                                     showGregorian)
import           Data.Time.Clock.POSIX              (getPOSIXTime)
import qualified Data.Vector                        as V
import           Network.HTTP.Simple                (getResponseBody,
                                                     httpSource, parseRequest)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

-- In gharchive, data is stored as gzipped JSON files for every hour since 2012.
--
-- Here we're iterating through every hour in 2017 and creating the urls to
-- download.
allUrls :: [String]
allUrls =
  [ "http://data.gharchive.org/" ++ d ++ "-" ++ t ++ ".json.gz"
  | d <- showGregorian <$> [fromGregorian 2017 1 1 .. fromGregorian 2017 31 31]
  , t <- show <$> [(0::Int)..23]
  ]

-- Entrance point.
main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend -> do
    -- For every url, create a separate thread which spawns a separate Lambda
    -- function.
    asyncs <- mapM
      (\u -> threadDelay 10000 >> async (spawn backend u))
      allUrls

    -- Wait and combine the results from different executors
    result <- mconcat <$> mapM wait asyncs

    -- Take the biggest 50 and print.
    result
      & M.toList . MM.getMonoidalMap
      & reverse . sortOn snd
      & mapM_ print . take 50

-- We actually don't need this much memory, since CPU increases linearly with
-- requested memory in AWS Lambda, more memory gives much better results, it even
-- is cheaper because functions finish more quickly.
opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-batch"
         & lboMemory .~ 1024


-- The function to spawns a single Lambda function.
spawn :: Backend -> String -> IO (MM.MonoidalMap T.Text (Sum Int))
spawn backend url = do
  putStrLn $ "Spawning function for: " ++ show url
  start <- getPOSIXTime

  -- Here's where the magic happens, we actually call the Lambda function and
  -- pass the url as a parameter.
  r <- execute
    backend
    (static Dict)
    (static processUrl `cap` cpure (static Dict) url)

  end <- getPOSIXTime

  putStrLn $ "Got response from " ++ show url ++
               " in " ++ show (end - start) ++ " seconds."
  return $ MM.MonoidalMap r

--------------------------------------------------------------------------------

-- Here, we make the actual HTTP results, extract, parse, and gather the results
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
