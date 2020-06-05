{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

module BatchTests where

import Conduit
import Control.Arrow
import Control.Distributed.Dataset
import Control.Distributed.Dataset.Local
import Control.Monad.Logger
import Data.Binary
import Data.Function
import Data.List (group, sort)
import Data.Typeable
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Haskell.TH (unType)

prop_aggrSum :: Property
prop_aggrSum =
  propTest
    (static Dict)
    (Gen.integral (Range.constant (0 :: Integer) 10))
    sum
    (dAggr (aggrSum (static Dict)))

prop_groupedAggrCount :: Property
prop_groupedAggrCount =
  propTest
    (static Dict)
    (Gen.enum 'a' 'z')
    ( sort
        >>> group
        >>> map (\xs@(x : _) -> (x, fromIntegral $ length xs))
    )
    ( dGroupedAggr 5 (static id) aggrCount
        >>> dToList
        >>> fmap sort
    )

prop_distinct :: Property
prop_distinct =
  propTest
    (static Dict)
    (Gen.enum 'a' 'z')
    ( sort
        >>> group
        >>> map head
    )
    ( dDistinct 10
        >>> dToList
        >>> fmap sort
    )

prop_groupedAggrBottomK :: Property
prop_groupedAggrBottomK =
  propTest
    (static Dict)
    (Gen.integral (Range.constant (0 :: Integer) 10))
    (take 5 . sort)
    (dAggr (aggrBottomK (static Dict) 5 (static id)))

propTest ::
  (Show a, Typeable a, StaticSerialise a, Eq b, Show b) =>
  Closure (Dict (Binary a, Typeable a)) ->
  GenT Identity a ->
  ([a] -> b) ->
  (Dataset a -> DD b) ->
  Property
propTest dict gen reference impl =
  property $ do
    input <-
      forAll $ Gen.list (Range.linear 0 10) $
        Gen.list
          (Range.constant 0 10)
          gen
    let expected = reference $ concat input
    actual <-
      liftIO . run $
        dExternal
          [ mkPartition
              ( static (\Dict -> mapM_ yield)
                  `cap` dict
                  `cap` cpure
                    (static (\Dict -> Dict) `cap` dict)
                    p
              )
            | p <- input
          ]
          & impl
    actual === expected

run :: DD a -> IO a
run dd =
  withLocalTmpShuffleStore $ \ss ->
    runDDWith LevelWarn localProcessBackend ss dd

batchTests :: Group
batchTests = $(unType <$> discover)
