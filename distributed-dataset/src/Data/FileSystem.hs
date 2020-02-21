{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FileSystem where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Proxy
import Path

class FS fs where

  {-# MINIMAL
    fsProtocols,
    fsLs,
    (fsReadFromOffset | fsReadFromOffsetLen)
    #-}

  type M fs :: * -> *

  fsProtocols ::
    Proxy fs ->
    [String]

  fsLs ::
    fs ->
    Path Abs Dir ->
    M fs ([Path Rel Dir], [Path Rel File])

  fsRead ::
    Monad (M fs) =>
    fs ->
    Path Abs File ->
    ConduitT () ByteString (M fs) ()
  fsRead fs =
    fsReadFromOffset fs 0

  fsReadFromOffset ::
    Monad (M fs) =>
    fs ->
    Int ->
    Path Abs File ->
    ConduitT () ByteString (M fs) ()
  fsReadFromOffset fs s p = produce s .| untilEmpty
    where
      produce s =
        fsReadFromOffsetLen fs s chunkSize p
          >> produce (s + chunkSize)
      untilEmpty = await >>= \case
        Just bs | not (BS.null bs) -> yield bs >> untilEmpty
        _ -> return ()
      chunkSize = 16 * 1024 * 1024

  fsReadFromOffsetLen ::
    Monad (M fs) =>
    fs ->
    Int ->
    Int ->
    Path Abs File ->
    ConduitT () ByteString (M fs) ()
  fsReadFromOffsetLen fs s l p =
    fsReadFromOffset fs s p .| untilLen 0
    where
      untilLen cur = do
        await >>= \case
          Nothing -> return ()
          Just bs ->
            let rem = l - cur
                len = BS.length bs
             in if len >= rem
                  then yield $ BS.take rem bs
                  else yield bs >> untilLen (cur + len)

class FS fs => WritableFS fs where

  fsWrite ::
    fs ->
    Path Abs File ->
    ConduitT ByteString () (M fs) ()

  fsRm ::
    fs ->
    Path Abs File ->
    m ()

  fsRmDir ::
    fs ->
    Path Abs Dir ->
    (M fs) ()

  fsRmDirRecursive ::
    Monad (M fs) =>
    fs ->
    Path Abs Dir ->
    (M fs) ()
  fsRmDirRecursive fs d = do
    (dirs, files) <- fsLs fs d
    mapM_ (fsRmDirRecursive fs . (d </>)) dirs
    mapM_ (fsRm fs . (d </>)) files
    fsRmDir fs d

  fsMkDir ::
    fs ->
    Path Abs Dir ->
    FilePath ->
    M fs ()

  fsMkDirParents ::
    fs ->
    Path Abs Dir ->
    M fs ()

doFileSystem :: String
doFileSystem = "FileSystem"
