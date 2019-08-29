{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module SerialiseTests where

--------------------------------------------------------------------------------
import Conduit
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
--------------------------------------------------------------------------------
import Data.Conduit.Serialise
import Data.IORef
import Data.List (sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Haskell.TH (unType)

--------------------------------------------------------------------------------
prop_serialiseEmpty :: Property
prop_serialiseEmpty =
  property $ do
    r <- liftIO . runConduitRes $ return () .| deserialiseC @Int .| sinkList
    r === []

prop_serialiseRoundtrip :: Property
prop_serialiseRoundtrip =
  property $ do
    xs <-
      forAll $ Gen.list (Range.linear 0 1000) $ do
        m <- Gen.maybe $ Gen.integral (Range.constant 0 1000)
        t <- Gen.bytes (Range.constant 0 100)
        return (m :: Maybe Int, t :: BS.ByteString)
    ch <- forAll $ Gen.integral (Range.constant 1 100)
    r <-
      liftIO . runConduitRes
        $ mapM_ yield xs
        .| serialiseC
        .| chunker ch
        .| deserialiseC
        .| sinkList
    r === xs

prop_serialiseWithLocRoundtrip :: Property
prop_serialiseWithLocRoundtrip =
  property $ do
    xs <-
      fmap sort . forAll $ Gen.list (Range.linear 0 1000) $ do
        k <- Gen.integral (Range.constant 0 1000)
        m <- Gen.maybe $ Gen.integral (Range.constant 0 1000)
        t <- Gen.bytes (Range.constant 0 100)
        return (k :: Int, (m :: Maybe Int, t :: BS.ByteString))
    ref <- liftIO $ newIORef []
    bs <-
      fmap BL.toStrict . liftIO . runConduitRes
        $ mapM_ yield xs
        .| (serialiseWithLocC >>= liftIO . writeIORef ref)
        .| sinkLazy
    ref' <- liftIO $ readIORef ref
    r <-
      fmap concat . forM ref' $ \(k, (from, to)) -> do
        assert $ from < to
        let slice = BS.take (fromIntegral $ to - from + 1) . BS.drop (fromIntegral from) $ bs
        fmap (map (k,)) . liftIO . runConduitRes
          $ yield slice
          .| deserialiseC
          .| sinkList
    r === xs

chunker :: Monad m => Int -> ConduitT BS.ByteString BS.ByteString m ()
chunker ch = go BS.empty
  where
    go l =
      if BS.length l >= ch
        then let (ret, new) = BS.splitAt ch l in yield ret >> go new
        else await >>= \case
          Nothing -> unless (BS.null l) (yield l)
          Just xs -> go (l <> xs)

serialiseTests :: Group
serialiseTests = $(unType <$> discover)
