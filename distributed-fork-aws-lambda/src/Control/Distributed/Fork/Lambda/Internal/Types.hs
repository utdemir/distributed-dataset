
{-# LANGUAGE OverloadedStrings #-}

module Control.Distributed.Fork.Lambda.Internal.Types where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
import Control.Applicative
import Control.Monad
--------------------------------------------------------------------------------

newtype BucketName = BucketName Text
  deriving (Eq, Show)

data S3Loc = S3Loc BucketName Text
  deriving (Eq, Show)

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Show)

data StackOptions =
  StackOptions { soName :: StackName
               , soLambdaMemory :: Int
               , soLambdaCode :: S3Loc
               }

data Response
  = Response Int ResponsePayload

data ResponsePayload
  = ResponsePayloadInline Text
  | ResponsePayloadS3 Text

instance FromJSON Response where
  parseJSON (Object obj) =
    Response
      <$> obj .: "id"
      <*> (parseInline obj <|> parseS3 obj)
    where
      parseInline o = do
        ty <- o .: "type"
        guard $ (ty :: Text) == "response-inline"
        ResponsePayloadInline <$> o .: "payload"
      parseS3 o = do
        ty <- o .: "type"
        guard $ (ty :: Text) == "response-s3"
        ResponsePayloadS3 <$> o .: "path"
  parseJSON _ = fail "unexpected response"
