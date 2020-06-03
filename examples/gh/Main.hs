{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TupleSections #-}

{-
This application will process every public GitHub event occured in 2018 to get
the top 20 users who used the word "cabal" in their commit messages the most.

This will download and process around 126 GB of compressed, 909 GB uncompressed
data; and it'll set you back a few dollars on your AWS bill.
-}
module Main where

import Control.Distributed.Dataset
import Control.Distributed.Dataset.AWS
import Control.Distributed.Dataset.OpenDatasets.GHArchive
import Control.Lens
import qualified Data.Text as T
import Data.Time.Clock
import System.Environment (getArgs)

app :: DD ()
app =
  ghArchive 10000 (fromGregorian 2019 01 25, fromGregorian 2020 04 01)
    & dMap
      ( static
          ( \e ->
              let time = e ^. gheCreatedAt
                  commits = e ^.. gheType . _GHPushEvent . ghpepCommits
               in (show $ utctDay time, length commits)
          )
      )
    & dGroupedAggr
      1
      (static fst)
      (static snd `staticLmap` aggrSum (static Dict))
    & dToList
    >>= mapM_ (\(d, n) -> liftIO . putStrLn $ d ++ " " ++ show n)

main :: IO ()
main = do
  initDistributedFork
  args <- getArgs
  bucket <-
    case args of
      [bucket'] -> return $ T.pack bucket'
      _ -> error "Usage: ./gh <bucket>"
  withLambdaBackend (lambdaBackendOptions bucket) $ \backend -> do
    let shuffleStore = s3ShuffleStore bucket "dd-shuffle-store/"
    runDD backend shuffleStore app
