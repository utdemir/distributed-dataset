module Network.Serverless.Execute.Lambda.Types where

--------------------------------------------------------------------------------
import Data.Text (Text)
--------------------------------------------------------------------------------

newtype BucketName = BucketName Text
  deriving (Eq, Show)

data S3Loc = S3Loc BucketName Text
  deriving (Eq, Show)

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Show)

newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Show)

newtype LambdaId = LambdaId Text
  deriving (Eq, Show)

data LambdaBackendOptions = LambdaBackendOptions
  { _lboBucket :: Text
  , _lboPrefix :: Text
  , _lboStackPrefix :: Text
  }
