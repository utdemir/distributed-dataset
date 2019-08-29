{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Conduit.Serialise where

import Codec.Serialise as S
import Conduit hiding (leftover)
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)

serialiseC :: forall a m. (Serialise a, Monad m) => ConduitM a ByteString m ()
serialiseC = mapC $ BL.toStrict . S.serialise

serialiseWithLocC :: forall a k m. (Eq k, Serialise a, Monad m) => ConduitM (k, a) ByteString m [(k, (Integer, Integer))]
serialiseWithLocC =
  go []
  where
    go acc =
      await >>= \case
        Nothing -> return $ reverse acc
        Just (k, a) -> do
          let ser = BL.toStrict $ S.serialise a
              len = fromIntegral $ BS.length ser
          yield ser
          case acc of
            [] -> go [(k, (0, len -1))]
            (k', (beg, end)) : rest ->
              if k' == k
                then go ((k', (beg, end + len)) : rest)
                else go ((k, (end + 1, end + len)) : acc)

deserialiseC :: forall a m. (Serialise a, MonadIO m) => ConduitM ByteString a m ()
deserialiseC = filterC (not . BS.null) .| pipe
  where
    pipe =
      fix
        ( \rec' -> \case
            Nothing ->
              await >>= \case
                Nothing -> return ()
                Just bs ->
                  liftIO (stToIO $ deserialiseIncremental @a) >>= \case
                    Fail {} ->
                      error "couldn't init parser"
                    Done {} ->
                      error "couldn't init parser"
                    Partial cont ->
                      liftIO (stToIO $ cont (Just bs)) >>= rec' . Just
            Just Fail {} ->
              lift $ error "failed"
            Just (Done leftover _ ret) -> do
              yield ret
              if BS.null leftover
                then rec' Nothing
                else liftIO (stToIO $ deserialiseIncremental @a) >>= \case
                  Fail {} ->
                    error "couldn't init parser"
                  Done {} ->
                    error "couldn't init parser"
                  Partial cont ->
                    liftIO (stToIO $ cont (Just leftover)) >>= rec' . Just
            Just (Partial cont) ->
              await >>= \case
                Nothing ->
                  liftIO (stToIO $ cont Nothing) >>= \case
                    Fail {} ->
                      error "parse failed"
                    Done _ _ ret ->
                      yield ret
                    Partial _ ->
                      error "parse failed. abrubt message?"
                Just bs ->
                  liftIO (stToIO $ cont (Just bs)) >>= rec' . Just
          )
        Nothing
