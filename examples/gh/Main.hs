{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TupleSections     #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Text                                          as T
import           System.Environment                                 (getArgs)
--------------------------------------------------------------------------------
import           Control.Distributed.Dataset
import           Control.Distributed.Dataset.AWS
import           Control.Distributed.Dataset.OpenDatasets.GHArchive
--------------------------------------------------------------------------------

app :: DD ()
app = 
  -- Fetch events from GitHub between given dates
  ghArchive (fromGregorian 2018 1 1, fromGregorian 2018 12 31)

    -- Extract every commit
    & dConcatMap (static (\e ->
        let author = e ^. gheActor . ghaLogin
            commits = e ^.. gheType . _GHPushEvent . ghpepCommits . traverse . ghcMessage
        in  map (author, ) commits
      ))

    -- Filter commits containing the word 'cabal'
    & dFilter (static (\(_, commit) ->
        T.pack "cabal" `T.isInfixOf` T.toLower commit
      ))

    -- Count the authors
    & dGroupedAggr 50 (static fst) dCount

    -- Fetch the top 20 to driver as a list
    & dAggr (dTopK (static Dict) 20 (static snd))

    -- Print them
    >>= mapM_ (liftIO . print)

main ::  IO ()
main = do
  initDistributedFork
  args <- getArgs
  bucket <- case args of
    [bucket'] -> return $ T.pack bucket'
    _         -> error "Usage: ./gh <bucket>"
  withLambdaBackend (lambdaBackendOptions bucket) $ \backend -> do
    let shuffleStore = s3ShuffleStore bucket "dd-shuffle-store/"
    runDD backend shuffleStore app
