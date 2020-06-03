{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}

module Control.Distributed.Dataset.OpenDatasets.GHArchive
  ( ghArchive,
    module Control.Distributed.Dataset.OpenDatasets.GHArchive.Types,

    -- * Re-exports
    fromGregorian,
  )
where

import Conduit
  ( (.|),
    ConduitT,
    ResourceT,
    handleC,
    throwM,
  )
import Control.Distributed.Dataset
import Control.Distributed.Dataset.OpenDatasets.GHArchive.Types
import Control.Exception
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.JSON.NewlineDelimited as NDJ
import Data.Conduit.Zlib (ungzip)
import qualified Data.Text as T
import Data.Time.Calendar
  ( Day,
    fromGregorian,
    showGregorian,
  )
import Data.Typeable
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpSource,
    parseRequest,
  )
import Text.Printf

ghArchive :: Int -> (Day, Day) -> Dataset GHEvent
ghArchive partitions (start, end) =
  dExternal
    ( map
        (\str -> mkPartition (static processUrl `cap` cpure (static Dict) str))
        (allUrls start end)
    )
    & dCoalesce partitions

allUrls :: Day -> Day -> [T.Text]
allUrls start end = do
  date <- showGregorian <$> [start .. end]
  time <- printf "%02d" <$> [(0 :: Int) .. 23]
  let str = T.pack date <> "-" <> T.pack time
  return $ "http://data.gharchive.org/" <> str <> ".json.gz"

processUrl :: T.Text -> ConduitT () GHEvent (ResourceT IO) ()
processUrl url =
  handleC wrapEx $ do
    req <- parseRequest (T.unpack url)
    httpSource req call
  where
    call req =
      case getResponseStatusCode req of
        200 ->
          getResponseBody req
            .| ungzip
            .| NDJ.eitherParser @_ @GHEvent
            .| C.mapM (either fail return)
        404 -> return ()
        r -> fail $ "Unexpected status code: " ++ show r
    wrapEx =
      throwM . GHArchiveException url . id @SomeException

data GHArchiveException e = GHArchiveException T.Text e
  deriving (Typeable, Show)

instance (Typeable e, Show e) => Exception (GHArchiveException e)
