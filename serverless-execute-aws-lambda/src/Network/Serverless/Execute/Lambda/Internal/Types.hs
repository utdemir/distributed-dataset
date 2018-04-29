{-# LANGUAGE OverloadedStrings #-}

module Network.Serverless.Execute.Lambda.Internal.Types where

--------------------------------------------------------------------------------
import Data.Text (Text)
--------------------------------------------------------------------------------

newtype BucketName = BucketName Text
  deriving (Eq, Show)

data S3Loc = S3Loc BucketName Text
  deriving (Eq, Show)

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Show)

newtype LambdaId = LambdaId Text
  deriving (Eq, Show)
