{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE DataKinds #-}

module Control.Distributed.Dataset.Parquet where

import qualified Data.Aeson                    as JSON
import           Control.Distributed.Dataset
import qualified Parquet.Reader                as P
import qualified Parquet.ThriftTypes           as P
import           Control.Lens                   ( (^.) )
import           GHC.Generics                   ( Generic )
import qualified Codec.Serialise               as Codec
import qualified Conduit                       as C

instance StaticSerialise P.ParquetValue where
  staticSerialise = static Dict

readParquet :: String -> P.FileMetadata -> Dataset P.ParquetValue
readParquet source metadata = dExternal
  (map
    (\rg -> mkPartition
      (     static P.sourceRowGroupFromRemoteFile
      `cap` cpure (static Dict) source
      `cap` cpure (static Dict) metadata
      `cap` cpure (static Dict) rg
      )
    )
    (metadata ^. P.pinchField @"row_groups")
  )
