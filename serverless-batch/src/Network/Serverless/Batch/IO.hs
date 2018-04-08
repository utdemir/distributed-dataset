{-# LANGUAGE DeriveGeneric #-}

module Network.Serverless.Batch.IO where

--------------------------------------------------------------------------------
import Pipes
import GHC.Generics
import qualified Data.ByteString as BS
--------------------------------------------------------------------------------

newtype Seek = Seek Integer
  deriving (Show, Eq, Ord, Generic)

data Source = Source
  { sourceRead :: FilePath -> Seek -> Producer BS.ByteString IO ()
  , sourceList :: FilePath -> IO [FilePath]
  , sourceFileSize :: FilePath -> IO (Maybe Integer)
  }

data Sink = Sink
  { sinkWrite :: FilePath -> Consumer BS.ByteString IO ()
  }

data FS = FS
  { fsSource :: Source
  , fsSink :: Sink
  }

--------------------------------------------------------------------------------
