{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FileSystem.InMemory
  ( InMemoryFS,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Either
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Control.Lens
import Path.Extra
import Data.FileSystem

data InMemoryFS
  = InMemoryFS (IORef Tree)

newtype Tree = Tree (Map String (Either Tree ByteString))

type instance Index Tree = String
instance Ixed Tree where
  ix p = undefined

instance FS InMemoryFS where

  type M InMemoryFS = IO

  fsProtocols Proxy = ["inmemory"]

  fsLs (InMemoryFS fs') target = do
    let (root, cs) = dirComponents target
    unless (toFilePath root == "/") $ fail "Root should be /"
    fs <- readIORef fs'
    Tree rs <-
      foldM
        ( \(Tree cur) c -> case toFilePath c `M.lookup` cur of
            Nothing -> fail $ "Not found: " ++ show c
            Just (Left d) -> return d
            Just (Right _) -> fail $ "Not a directory: " ++ show c
        )
        fs
        cs
    (\(f, s) -> (,) <$> sequence f <*> sequence s)
      . partitionEithers
      . map
        ( \(s, e) ->
            bimap
              (const $ parseRelDir s)
              (const $ parseRelFile s)
              e
        )
      . M.toList
      $ rs

  fsReadFromOffsetLen (InMemoryFS fs') start len target = do
    let (root, cs, fpath) = fileComponents target
    unless (toFilePath root == "/") $ fail "Root should be /"
    fs <- liftIO $ readIORef fs'
    Tree rs <-
      foldM
        ( \(Tree cur) c -> case toFilePath c `M.lookup` cur of
            Nothing -> fail $ "Not found: " ++ show c
            Just (Left d) -> return d
            Just (Right _) -> fail $ "Not a directory: " ++ show c
        )
        fs
        cs
    undefined
