{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Dataset.AWS
  ( s3ShuffleStore,
    -- Re-exports
    module Control.Distributed.Fork.AWS
    )
where

--------------------------------------------------------------------------------
import Conduit
import Control.Distributed.Closure
--------------------------------------------------------------------------------
import Control.Distributed.Dataset.ShuffleStore
import Control.Distributed.Fork.AWS
import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS (AWST)
import qualified Data.Text as T
import Network.AWS
import Control.Monad.Fail
import Network.AWS.Data.Body (RsBody (_streamBody))
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.S3.StreamingUpload as S3
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- |
-- A shuffle store which uses given S3 bucket and the prefix as a shuffle store.
--
-- TODO: Cleanup
-- TODO: Use a temporary bucket created by CloudFormation
s3ShuffleStore :: T.Text -> T.Text -> ShuffleStore
s3ShuffleStore bucket' prefix' =
  ShuffleStore
    { ssGet =
        static
          ( \bucket prefix num range -> do
              ret <-
                runAWS globalAWSEnv
                  $ send
                  $ S3.getObject
                      (S3.BucketName bucket)
                      (S3.ObjectKey $ prefix <> T.pack (show num))
                  & S3.goRange
                  .~ ( case range of
                         RangeAll -> Nothing
                         RangeOnly lo hi ->
                           Just . T.pack $ "bytes=" <> show lo <> "-" <> show hi
                       )
              _streamBody $ ret ^. S3.gorsBody
            )
          `cap` cpure (static Dict) bucket'
          `cap` cpure (static Dict) prefix',
      ssPut =
        static
          ( \bucket prefix num ->
              void . transPipe @(AWST (ResourceT IO)) (runAWS globalAWSEnv)
                $ S3.streamUpload Nothing
                $ S3.createMultipartUpload
                    (S3.BucketName bucket)
                    (S3.ObjectKey $ prefix <> T.pack (show num))
            )
          `cap` cpure (static Dict) bucket'
          `cap` cpure (static Dict) prefix'
      }

instance MonadIO m => MonadFail (AWST m) where
  fail = lift . Prelude.fail

-- FIXME
-- Currently ShuffleStore does not allow storing data between the 'ssGet's.
-- Since this function is quite costly, we're using this horrible hack.
--
-- Since using a state on 'get's is pretty common (having a database connection,
-- authentication etc.) we should figure out a way to fix this.
globalAWSEnv :: Env
globalAWSEnv = unsafePerformIO $ newEnv Discover
{-# NOINLINE globalAWSEnv #-}
