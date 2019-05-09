{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Distributed.Dataset.Internal.Dataset where

-------------------------------------------------------------------------------
import           Conduit                                      hiding (Consumer,
                                                               Producer, await)
import qualified Conduit                                      as C
import           Control.Distributed.Closure
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Hashable
import qualified Data.IntMap                                  as M
import qualified Data.IntMap.Merge.Strict                     as M
import           Data.IORef
import           Data.List                                    (foldl', sortOn,
                                                               transpose)
import           Data.List.Split
import qualified Data.Text                                    as T
import           Data.Typeable
import           System.Random
-------------------------------------------------------------------------------
import           Control.Distributed.Dataset.Internal.Class
import           Control.Distributed.Dataset.Internal.Process
import           Control.Distributed.Dataset.ShuffleStore
import qualified Control.Distributed.Fork.Utils               as D
import           Data.Conduit.Serialise
-------------------------------------------------------------------------------

-- * Partition

-- |
-- Represents some amount of data which to be transformed on a single
-- executor.
data Partition a where
  PEmpty :: Partition a
  PSimple :: Closure (ConduitT () a (ResourceT IO) ()) -> Partition a
  PCombined :: Partition a -> Partition a -> Partition a

instance Semigroup (Partition a) where
  a <> b = PCombined a b

instance Monoid (Partition a) where
  mempty = PEmpty

-- |
-- Streams the elements from a 'Partition'.
partitionProducer :: Typeable a => Partition a -> Closure (ConduitT () a (ResourceT IO) ())
partitionProducer PEmpty = static (return ())
partitionProducer (PSimple p) = p
partitionProducer (PCombined p1 p2) =
  static (>>) `cap` partitionProducer p1 `cap` partitionProducer p2

-- * Dataset

-- |
-- Represents a partitioned multiset that can be transformed in a distributed fashion.
data Dataset a where
  DExternal  :: StaticSerialise a => [Partition a] -> Dataset a
  DPipe      :: (StaticSerialise a, StaticSerialise b)
             => Closure (ConduitT a b (ResourceT IO) ())
             -> Dataset a -> Dataset b
  DPartition :: (StaticHashable k, StaticSerialise a)
             => Int
             -> Closure (a -> k)
             -> Dataset a -> Dataset a
  DCoalesce  :: Int
             -> Dataset a
             -> Dataset a

dStaticSerialise :: Dataset a -> Dict (StaticSerialise a)
dStaticSerialise DExternal{}     = Dict
dStaticSerialise DPipe{}         = Dict
dStaticSerialise DPartition{}    = Dict
dStaticSerialise (DCoalesce _ d) = dStaticSerialise d

-- * Stage

data Stage a where
  SInit :: Typeable a => [Partition a] -> Stage a
  SNarrow :: (StaticSerialise a, StaticSerialise b)
          => Closure (ConduitM a b (ResourceT IO) ()) -> Stage a -> Stage b
  SWide :: (StaticSerialise a, StaticSerialise b)
        => Int -> Closure (ConduitM a (Int, b) (ResourceT IO) ()) -> Stage a -> Stage b
  SCoalesce :: Int -> Stage a -> Stage a

instance Show (Stage a) where
  show s@(SInit _)       =  showTopStage s
  show s@(SNarrow _ r)   = mconcat [show r, " -> " , showTopStage s]
  show s@(SWide _ _ r)   = mconcat [show r, " -> " , showTopStage s]
  show s@(SCoalesce _ r) = mconcat [show r, " -> " , showTopStage s]

showTopStage :: forall a. Stage a -> String
showTopStage (SInit p) = mconcat
  [ "SInit"
  , " @", show (typeRep $ Proxy @a)
  , " ", show (length p)
  ]
showTopStage (SNarrow _ _) = mconcat
  [ "* SNarrow"
  , " @", show (typeRep $ Proxy @a)
  ]
showTopStage (SWide i _ _) = mconcat
  [ "* SWide"
  , " @", show (typeRep $ Proxy @a)
  , " ", show i
  ]
showTopStage (SCoalesce i _) = mconcat
  [ "SCoalesce"
  , " ", show i
  ]

mkStages :: Dataset a -> Stage a
mkStages (DExternal a) = SInit a
mkStages (DPipe p rest) =
  case mkStages rest of
    SNarrow prev r ->
      SNarrow (static (.|) `cap` prev `cap` p) r
    other ->
      SNarrow p other
mkStages (DPartition count (cf :: Closure (a -> k)) rest) =
  case mkStages rest of
    SNarrow cp rest' ->
      SWide count
            (static (\Dict p f -> p .| partition @a @k f)
               `cap` staticHashable @k
               `cap` cp
               `cap` cf)
            rest'
    other ->
      SWide count
            (static (\Dict -> partition @a @k)
              `cap` staticHashable @k
              `cap` cf
            )
            other
  where
    partition :: forall t e m. (Hashable e, Monad m) => (t -> e) -> ConduitT t (Int, t) m ()
    partition f =
      C.await >>= \case
        Nothing -> return ()
        Just a  -> C.yield (hash (f a), a) >> partition f
mkStages (DCoalesce count rest) =
  case mkStages rest of
    SNarrow cp rest' ->
      SNarrow cp (SCoalesce count rest')
    SWide _ cp rest' ->
      SWide count cp rest'
    SCoalesce _ rest' ->
      SCoalesce count rest'
    other ->
      SCoalesce count other


-- FIXME: This function should not look this horrible.
runStages :: forall a. Stage a -> DD [Partition a]
runStages stage@(SInit ps) = do
  logInfoN $ "Running: " <> T.pack (showTopStage stage)
  return ps

runStages stage@(SNarrow cpipe rest) = do
  inputs <- runStages rest
  logInfoN $ "Running: " <> T.pack (showTopStage stage)
  shuffleStore <- view ddShuffleStore
  tasks <- forM inputs $ \input -> do
    num <- liftIO randomIO
    let coutput = ssPut shuffleStore `cap` cpure (static Dict) num
        cinput = ssGet shuffleStore `cap` cpure (static Dict) num `cap` cpure (static Dict) RangeAll
        crun = static (\Dict producer pipe output ->
          withExecutorStats $ \ExecutorStatsHooks{..} ->
            C.runConduitRes
              $ producer
                  .| eshInput
                  .| pipe
                  .| eshOutput
                  .| serialiseC @a
                  .| eshUpload
                  .| output
          ) `cap` staticSerialise @a `cap` partitionProducer input `cap` cpipe `cap` coutput
        newPartition = PSimple @a (static (\Dict input' -> input' .| deserialiseC)
                           `cap` staticSerialise @a `cap` cinput)
    return (crun, newPartition)
  backend <- view ddBackend
  level <- view ddLogLevel
  let f = D.forkConcurrently
            (D.defaultOptions
              { D.oShowProgress = level <= LevelInfo
              , D.oRetries = 2
              })
  ret <- liftIO $ f backend (static Dict) (map fst tasks)
  logDebugN $ "Stats: " <> T.pack (show $ foldMap erStats ret)
  return $ map snd tasks

runStages stage@(SWide count cpipe rest) = do
  inputs <- runStages rest
  logInfoN $ "Running: " <> T.pack (showTopStage stage)
  shuffleStore <- view ddShuffleStore
  tasks <- forM inputs $ \partition -> do
    num <- liftIO randomIO
    let coutput = ssPut shuffleStore `cap` cpure (static Dict) num
        crun = static (\Dict count' input pipe output ->
          withExecutorStats $ \ExecutorStatsHooks{..} -> do
            ref <- newIORef @[(Int, (Integer, Integer))] []
            C.runConduitRes $
              input
                  .| eshInput
                  .| pipe
                  .| mapC (\(k, v) -> (k `mod` count', v))
                  .| eshOutput
                  .| sort @(ResourceT IO)
                  .| (serialiseWithLocC @a @Int @(ResourceT IO) >>= liftIO . writeIORef ref)
                  .| eshUpload
                  .| output
            readIORef ref
          ) `cap` staticSerialise @a
            `cap` cpure (static Dict) count
            `cap` partitionProducer partition
            `cap` cpipe
            `cap` coutput
    return (crun, num)

  backend <- view ddBackend
  level <- view ddLogLevel
  let f = D.forkConcurrently
            (D.defaultOptions
              { D.oShowProgress = level <= LevelInfo
              , D.oRetries = 2
              })
  ret <- liftIO $ f backend (static Dict) (map fst tasks)
  logDebugN $ "Stats: " <> T.pack (show $ foldMap erStats ret)

  let ret' = zip (map erResponse ret) (map snd tasks)

  partitions <- forM ret' $ \(res, num) ->
    forM res $ \(partition, (start, end)) ->
      return ( partition
             , PSimple @a (static (\Dict input' -> input' .| deserialiseC)
                           `cap` staticSerialise @a
                           `cap` (ssGet shuffleStore
                                    `cap` cpure (static Dict) num
                                    `cap` cpure (static Dict) (RangeOnly start end)
                                 )
                          )
             )

  map M.fromList partitions
    & foldl' (M.merge M.preserveMissing M.preserveMissing (M.zipWithMatched $ const mappend)) M.empty
    & M.toList
    & map snd
    & return

  where
    sort :: Monad m => ConduitT (Int, t) (Int, t) m ()
    sort = mapM_ yield . sortOn fst =<< C.sinkList

runStages stage@(SCoalesce count rest) = do
  inputs <- runStages rest
  logInfoN $ "Running: " <> T.pack (showTopStage stage)
  return $ map mconcat $ transpose (chunksOf count inputs)

-- * Dataset API

-- |
-- Returns a Conduit to fetch the results lazily to the driver.
dFetch :: StaticSerialise a
       => Dataset a
       -> DD (ConduitT () a (ResourceT IO) ())
dFetch ds = do
  let stages = mkStages ds
  logInfoN $ "Stages: " <> T.pack (show stages)
  out <- runStages stages
  return $ mapM_ (unclosure . partitionProducer) out

-- |
-- Fetches the complete dataset as a list.
dToList :: StaticSerialise a
        => Dataset a
        -> DD [a]
dToList ds = do
  c <- dFetch ds
  liftIO $ runConduitRes $ c .| sinkList
