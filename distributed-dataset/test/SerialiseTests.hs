{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}

module SerialiseTests where

--------------------------------------------------------------------------------
import           Codec.Serialise
import           Conduit
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Function
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Data.Conduit.Serialise (deserialiseC)
--------------------------------------------------------------------------------

serialiseTests :: TestTree
serialiseTests = testGroup "Serialise"
  [ testGroup
      "deserialise"
      [ testCase "empty" $ do
        r <- runConduitRes $ return () .| deserialiseC @Int .| sinkList
        r @=? []
      , testCase "single" $ do
        r <-
          runConduitRes
          $  yield (BL.toStrict $ serialise @Int 42)
          .| deserialiseC @Int
          .| sinkList
        r @=? [42]
      , testCase "multiple" $ do
        r <-
          runConduitRes
          $  mapM_
               yield
               [ BL.toStrict $ serialise @Int 42
               , BL.toStrict $ serialise @Int 43
               , BL.toStrict $ serialise @Int 44
               ]
          .| deserialiseC @Int
          .| sinkList
        r @=? [42, 43, 44]
      , testCase "partial" $ do
        let strs    = map show [(1 :: Int) .. 100000]
            encoded = mconcat $ map (BL.toStrict . serialise) strs
            split   = reverse $ fix
              (\rec rem' acc -> case BS.splitAt 4 rem' of
                (x, rest) ->
                  if BS.null rest then (x : acc) else rec rest (x : acc)
              )
              encoded
              []
        r <-
          runConduitRes
          $  mapM_ yield split
          .| deserialiseC @String
          .| sinkList
        r @=? strs
      ]
  ]
