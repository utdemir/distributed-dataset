{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Distributed.Dataset.OpenDatasets.GHArchive.Types where

import Codec.Serialise (Serialise)
import Control.Distributed.Dataset
  ( Dict (Dict),
    StaticSerialise (staticSerialise),
  )
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson ((.!=), (.:), (.:?), FromJSON (..), withObject)
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)

data GHEvent
  = GHEvent
      { _gheId :: Text,
        _gheCreatedAt :: UTCTime,
        _gheActor :: GHActor,
        _gheRepo :: GHRepo,
        _gheType :: GHEventType
      }
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHEvent where
  staticSerialise = static Dict

instance FromJSON GHEvent where
  parseJSON val =
    withObject
      "GHEvent"
      ( \obj ->
          GHEvent
            <$> obj .: "id"
            <*> (obj .: "created_at" >>= tp)
            <*> obj .: "actor"
            <*> obj .: "repo"
            <*> parseJSON val
      )
      val
    where
      tp =
        parseTimeM
          False
          defaultTimeLocale
          "%Y-%m-%dT%H:%M:%S%Z" -- Ex: 2019-05-15T15:20:53Z

data GHEventType
  = GHPushEvent GHPushEventPayload
  | GHOtherEvent Text
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHEventType where
  staticSerialise = static Dict

instance FromJSON GHEventType where
  parseJSON =
    withObject
      "GHEventType"
      ( \obj -> do
          ty <- obj .: "type"
          payload <- obj .: "payload"
          case ty of
            "PushEvent" -> GHPushEvent <$> parseJSON payload
            other -> return $ GHOtherEvent other
      )

newtype GHPushEventPayload = GHPushEventPayload {_ghpepCommits :: [GHCommit]}
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHPushEventPayload where
  staticSerialise = static Dict

instance FromJSON GHPushEventPayload where
  parseJSON =
    withObject "GHPushEventPayload" $ \obj ->
      GHPushEventPayload
        <$> obj .: "commits"

data GHCommit
  = GHCommit
      { _ghcAuthor :: GHCommitAuthor,
        _ghcMessage :: Text,
        _ghcSha :: Text,
        _ghcDistinct :: Bool
      }
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHCommit where
  staticSerialise = static Dict

instance FromJSON GHCommit where
  parseJSON =
    withObject "GHCommit" $ \obj ->
      GHCommit
        <$> obj .: "author"
        <*> obj .: "message"
        <*> obj .: "sha"
        <*> obj .:? "distinct" .!= True

data GHCommitAuthor = GHCommitAuthor {_ghcaEmail :: Text, _ghcaName :: Text}
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHCommitAuthor where
  staticSerialise = static Dict

instance FromJSON GHCommitAuthor where
  parseJSON =
    withObject "GHCommitAuthor" $ \obj ->
      GHCommitAuthor
        <$> obj .: "email"
        <*> obj .: "name"

data GHActor = GHActor {_ghaId :: Integer, _ghaLogin :: Text}
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHActor where
  staticSerialise = static Dict

instance FromJSON GHActor where
  parseJSON =
    withObject "GHActor" $ \obj ->
      GHActor
        <$> obj .: "id"
        <*> obj .: "login"

data GHRepo = GHRepo {_ghrId :: Integer, _ghrName :: Text}
  deriving (Eq, Show, Generic, Serialise)

instance StaticSerialise GHRepo where
  staticSerialise = static Dict

instance FromJSON GHRepo where
  parseJSON =
    withObject "GHRepo" $ \obj ->
      GHRepo
        <$> obj .: "id"
        <*> obj .: "name"

makeLenses ''GHEvent

makePrisms ''GHEventType

makeLenses ''GHPushEventPayload

makeLenses ''GHCommit

makeLenses ''GHCommitAuthor

makeLenses ''GHActor

makeLenses ''GHRepo
