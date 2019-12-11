{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TupleSections #-}

{-
This application will process every public GitHub event occured in 2018 to get
the top 20 users who used the word "cabal" in their commit messages the most.

This will download and process around 126 GB of compressed, 909 GB uncompressed
data; and it'll set you back a few dollars on your AWS bill.
-}
module Main where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import           Control.Distributed.Dataset
import           Control.Distributed.Dataset.Parquet
import           Control.Distributed.Dataset.AWS
                                               as AWS
import qualified Data.Text                     as T
import           Control.Distributed.Dataset.AWS
                                               as AWS
import qualified Network.AWS.S3                as S3
import           Control.Monad.IO.Class         ( liftIO )
import qualified Parquet.Reader                as P
import           Control.Distributed.Fork.Local
import           Control.Distributed.Dataset.Local
import           System.Environment             ( getArgs )
import           Control.Monad.Except
import qualified Conduit                       as C

--------------------------------------------------------------------------------
app :: DD ()
app = do
  let filename =
        "https://yigitozkavci-dd-test-bucket.s3.amazonaws.com/test.parquet"
  C.runResourceT (runExceptT (P.readMetadata (P.remoteParquetFile filename)))
    >>= \case
          Left  _        -> fail "wow"
          Right metadata -> do
            liftIO $ print metadata
            readParquet filename metadata & dToList >>= mapM_ (liftIO . print) -- Print them

main :: IO ()
main = do
  initDistributedFork
  args <- getArgs
  withLocalTmpShuffleStore
    $ \shuffleStore -> runDD localProcessBackend shuffleStore app
