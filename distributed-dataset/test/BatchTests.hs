{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}

module BatchTests where

--------------------------------------------------------------------------------
import           Conduit
import           Control.Monad.Logger
import           Data.Function
import           Data.List                                        (group, sort)
import           Hedgehog
import qualified Hedgehog.Gen                                     as Gen
import qualified Hedgehog.Range                                   as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------
import           Control.Distributed.Dataset
import           Control.Distributed.Dataset.LocalTmpShuffleStore
--------------------------------------------------------------------------------

datasetTests :: TestTree
datasetTests = testGroup "BatchTests"
  [ testCase "empty" $ do
      ret <- run $
        dExternal @Int [mkPartition (static (return ()))]
          & dToList
      ret @?= []
  , testCase "empty/count" $ do
      ret <- run $
        dExternal @Int [mkPartition (static (return ()))]
          & dAggr dCount
      ret @?= 0
  , testCase "aggr1" $ do
      ret <- run $
        dExternal @Int
            [ mkPartition (static (mapM_ yield [1..5]))
            , mkPartition (static (mapM_ yield [6..10]))
            ]
          & dAggr dCount
      ret @?= 10
  , testCase "aggr2" $ do
      ret <- run $
        dExternal @Int
          [ mkPartition (static (mapM_ yield [1..5]))
          , mkPartition (static (mapM_ yield [6..10]))
          ]
        & dAggr (dSum (static Dict))
      ret @?= 55
  , testProperty "prop_aggr" $ property $ do
      input <- forAll $ Gen.list (Range.linear 0 200) $
        Gen.list (Range.linear 0 1000) $
          Gen.integral (Range.constant 0 1000)
      let expected = sum . concat $ input
      ret <- liftIO . run $
        dExternal @Integer
          [ mkPartition (static (mapM_ yield) `cap` cpure (static Dict) p)
          | p <- input
          ]
        & dAggr (dSum (static Dict))
      ret === expected
  , testProperty "prop_groupedAggr" $ property $ do
      input <- forAll $ Gen.list (Range.linear 0 200) $
        Gen.list (Range.linear 0 1000) $
          Gen.enum 'a' 'z'
      let expected =
            map (\xs@(x:_) -> (x, fromIntegral $ length xs))
            . group . sort . concat
            $ input
      ret <- liftIO . run $
        dExternal @Char
          [ mkPartition (static (mapM_ yield) `cap` cpure (static Dict) p)
          | p <- input
          ]
        & dGroupedAggr 5 (static id) dCount
        & dToList
        & fmap sort

      ret === expected
  , testProperty "prop_bottomk" $ property $ do
      input <- forAll $ Gen.list (Range.linear 0 200) $
        Gen.list (Range.constant 0 1000) $
          Gen.integral (Range.constant 0 1000)
      let expected = take 5 . sort . concat $ input
      ret <- liftIO . run $
        dExternal @Integer
          [ mkPartition (static (mapM_ yield) `cap` cpure (static Dict) p)
          | p <- input
          ]
        & dAggr (dBottomK (static Dict) 5 (static id))
      ret === expected
  ]

run :: DD a -> IO a
run dd =
  withLocalTmpShuffleStore $ \ss ->
    runDDWith LevelWarn localProcessBackend ss dd
