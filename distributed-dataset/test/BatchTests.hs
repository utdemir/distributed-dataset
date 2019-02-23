{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}

module BatchTests where

--------------------------------------------------------------------------------
import           Conduit
import           Data.Function
import           Data.List                                        (sort)
import           Test.Tasty
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
  , testCase "groupedAggr" $ do
      ret <- run $
        dExternal @Char
          [ mkPartition (static (mapM_ yield ['a', 'b']))
          , mkPartition (static (mapM_ yield ['b', 'c', 'd']))
          , mkPartition (static (mapM_ yield ['d']))
          ]
        & dGroupedAggr 5 (static id) dCount
        & dToList
        & fmap sort
      ret @?= [('a', 1), ('b', 2), ('c', 1), ('d', 2)]
  ]

run :: DD a -> IO a
run dd =
  withLocalTmpShuffleStore $ \ss ->
    runDD localProcessBackend ss dd
