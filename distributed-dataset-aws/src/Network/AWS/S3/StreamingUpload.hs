{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- This is copied & modified from the package 'amazonka-s3-streaming'.
--
-- We should use that one instead once it's compatible with conduit 1.3.
module Network.AWS.S3.StreamingUpload
(
    streamUpload
    , abortAllUploads
    , module Network.AWS.S3.CreateMultipartUpload
    , module Network.AWS.S3.CompleteMultipartUpload
    , minimumChunkSize
) where

import           Conduit                                (ConduitT,
                                                         MonadResource,
                                                         MonadUnliftIO, Void,
                                                         await, catchC, throwM)
import           Control.Applicative
import           Control.Category                       ((>>>))
import           Control.DeepSeq                        (rnf)
import           Control.Lens                           (set, view)
import           Control.Lens.Operators
import           Control.Monad                          (forM_, when, (>=>))
import           Control.Monad.Catch                    (SomeException)
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Morph                    (lift)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString                        as BS
import           Data.ByteString.Builder                (stringUtf8)
import           Data.Conduit.List                      (sourceList)
import qualified Data.DList                             as D
import           Data.List.NonEmpty                     (nonEmpty)
import           Network.AWS                            (HasEnv (..),
                                                         LogLevel (..),
                                                         MonadAWS, hashedBody,
                                                         liftAWS, send, toBody)
import           Network.AWS.Data.Crypto                (Digest, SHA256,
                                                         hashFinalize, hashInit,
                                                         hashUpdate)
import           Network.AWS.S3.AbortMultipartUpload
import           Network.AWS.S3.CompleteMultipartUpload
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.ListMultipartUploads
import           Network.AWS.S3.Types                   (BucketName, cmuParts, completedMultipartUpload,
                                                         completedPart, muKey,
                                                         muUploadId)
import           Network.AWS.S3.UploadPart
import           System.Mem                             (performGC)
import           Text.Printf                            (printf)


type ChunkSize = Int

-- | Minimum size of data which will be sent in a single part, currently 6MB
minimumChunkSize :: ChunkSize
minimumChunkSize = 6*1024*1024 -- Making this 5MB+1 seemed to cause AWS to complain


{- |
Given a 'CreateMultipartUpload', creates a 'Sink' which will sequentially
upload the data streamed in in chunks of at least 'chunkSize' and return
the 'CompleteMultipartUploadResponse'.

If uploading of any parts fails, an attempt is made to abort the Multipart
upload, but it is likely that if an upload fails this abort may also fail.
'Network.AWS.S3.ListMultipartUploads' can be used to list any pending
uploads - it is important to abort multipart uploads because you will
be charged for storage of the parts until it is completed or aborted.
See the AWS documentation for more details.

May throw 'Network.AWS.Error'
-}
streamUpload :: (MonadResource m, MonadAWS m, MonadUnliftIO m)
             => Maybe ChunkSize -- ^ Optional chunk size
             -> CreateMultipartUpload -- ^ Upload location
             -> ConduitT ByteString Void m CompleteMultipartUploadResponse
streamUpload mcs cmu = do
  logger <- lift $ liftAWS $ view envLogger
  let logStr :: MonadIO m => String -> m ()
      logStr = liftIO . logger Debug . stringUtf8
      chunkSize = maybe minimumChunkSize (max minimumChunkSize) mcs

  cmur <- lift $ send cmu
  when (cmur ^. cmursResponseStatus /= 200) $
    fail "Failed to create upload"

  logStr "\n**** Created upload\n"

  let Just upId = cmur ^. cmursUploadId
      bucket    = cmu  ^. cmuBucket
      key       = cmu  ^. cmuKey
      -- go :: Text -> Builder -> Int -> Int -> Sink ByteString m ()
      go !bss !bufsize !ctx !partnum !completed = await >>= \mbs -> case mbs of
        Just bs | l <- BS.length bs
                , bufsize + l <= chunkSize ->
                    go (D.snoc bss bs) (bufsize + l) (hashUpdate ctx bs) partnum completed

                | otherwise -> do
                    rs <- lift $ partUploader partnum (bufsize + BS.length bs)
                                              (hashFinalize $ hashUpdate ctx bs)
                                              (D.snoc bss bs)

                    logStr $ printf "\n**** Uploaded part %d size %d\n" partnum bufsize
                    let part = completedPart partnum <$> (rs ^. uprsETag)
#if MIN_VERSION_amazonka_s3(1,4,1)
                        !_ = rnf part
#endif
                    liftIO performGC
                    go empty 0 hashInit (partnum+1) . D.snoc completed $! part

        Nothing -> lift $ do
            prts <- if True -- bufsize > 0
                then do
                    rs <- partUploader partnum bufsize (hashFinalize ctx) bss

                    logStr $ printf "\n**** Uploaded (final) part %d size %d\n" partnum bufsize

                    let allParts = D.toList $ D.snoc completed $ completedPart partnum <$> (rs ^. uprsETag)
                    pure $ nonEmpty =<< sequence allParts
                else do
                    logStr $ printf "\n**** No final data to upload\n"
                    pure $ nonEmpty =<< sequence (D.toList completed)

            send $ completeMultipartUpload bucket key upId
                    & cMultipartUpload ?~ set cmuParts prts completedMultipartUpload


      partUploader :: MonadAWS m => Int -> Int -> Digest SHA256 -> D.DList ByteString -> m UploadPartResponse
      partUploader pnum size digest =
        D.toList
        >>> sourceList
        >>> hashedBody digest (fromIntegral size)
        >>> toBody
        >>> uploadPart bucket key pnum upId
        >>> send
        >=> checkUpload

      checkUpload :: (Monad m) => UploadPartResponse -> m UploadPartResponse
      checkUpload upr = do
        when (upr ^. uprsResponseStatus /= 200) $ fail "Failed to upload piece"
        return upr

  catchC (go D.empty 0 hashInit 1 D.empty) (\ex ->
    lift (send (abortMultipartUpload bucket key upId)) >> throwM (ex :: SomeException))
      -- Whatever happens, we abort the upload and rethrow


-- | Aborts all uploads in a given bucket - useful for cleaning up.
abortAllUploads :: MonadAWS m => BucketName -> m ()
abortAllUploads bucket = do
  rs <- send (listMultipartUploads bucket)
  forM_ (rs ^. lmursUploads) $ \mu -> do
    let mki = (,) <$> mu ^. muKey <*> mu ^. muUploadId
    case mki of
      Nothing        -> pure ()
      Just (key,uid) -> send (abortMultipartUpload bucket key uid) >> pure ()



