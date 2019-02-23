{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeApplications  #-}

module Control.Distributed.Dataset.OpenDatasets.GHArchive
    ( ghArchive
    , module Control.Distributed.Dataset.OpenDatasets.GHArchive.Types
    -- * Re-exports
    , fromGregorian
    ) where

--------------------------------------------------------------------------------
import           Conduit                                                  (ConduitT,
                                                                           ResourceT,
                                                                           (.|))
import qualified Data.Conduit.Combinators                                 as C
import qualified Data.Conduit.JSON.NewlineDelimited                       as NDJ
import           Data.Conduit.Zlib                                        (ungzip)
import           Data.Time.Calendar                                       (Day, fromGregorian,
                                                                           showGregorian)
import           Network.HTTP.Simple                                      (getResponseBody,
                                                                           httpSource,
                                                                           parseRequest)
--------------------------------------------------------------------------------
import           Control.Distributed.Dataset
import           Control.Distributed.Dataset.OpenDatasets.GHArchive.Types
--------------------------------------------------------------------------------

ghArchive :: (Day, Day) -> Dataset GHEvent
ghArchive (start, end) =
  dExternal
    ( map (\str -> mkPartition (static processUrl `cap` cpure (static Dict) str))
          (allUrls start end)
    ) & dCoalesce ((fromEnum end - fromEnum start + 1) * 6)

allUrls :: Day -> Day -> [String]
allUrls start end = do
  date <- showGregorian <$> [start..end]
  time <- show <$> [(0::Int)..23]
  return $ "http://data.gharchive.org/" ++ date ++ "-" ++ time ++ ".json.gz"

processUrl :: String -> ConduitT () GHEvent (ResourceT IO) ()
processUrl url = do
  req <- parseRequest url
  httpSource req getResponseBody
    .| ungzip
    .| NDJ.eitherParser @_ @GHEvent
    .| C.mapM (either fail return)

