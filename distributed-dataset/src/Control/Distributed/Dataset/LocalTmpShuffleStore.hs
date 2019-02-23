{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE StaticPointers   #-}
{-# LANGUAGE TypeApplications #-}

module Control.Distributed.Dataset.LocalTmpShuffleStore
  ( withLocalTmpShuffleStore
  -- * Re-exports
  , localProcessBackend
  ) where

--------------------------------------------------------------------------------
import           Conduit
import           Control.Distributed.Closure
import           Control.Exception                            (bracket)
import qualified Data.ByteString                              as BS
import qualified Data.Conduit.Combinators                     as C
import           Prelude                                      hiding (rem)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Posix.Temp
--------------------------------------------------------------------------------
import           Control.Distributed.Dataset.ShuffleStore
import           Control.Distributed.Fork.LocalProcessBackend
--------------------------------------------------------------------------------

-- |
-- Uses a temporary directory in the local disk as a 'ShuffleStore'.
--
-- Useful for testing purposes.
withLocalTmpShuffleStore :: (ShuffleStore -> IO a) -> IO a
withLocalTmpShuffleStore act = do
  bracket
    (mkdtemp "/tmp/dd-" >>= \p -> print p >> return p)
    removeDirectoryRecursive
    (act . mk)
  where
    mk :: String -> ShuffleStore
    mk tmp' = ShuffleStore
      { ssGet = static (\tmp int range -> streamFile range (tmp </> show int)) `cap` cpure (static Dict) tmp'
      , ssPut = static (\tmp int -> C.sinkFile (tmp </> show int)) `cap` cpure (static Dict) tmp'
      }

streamFile :: Range -> FilePath -> ConduitT t BS.ByteString (ResourceT IO) ()
streamFile RangeAll fp = C.sourceFile fp
streamFile (RangeOnly start end) _ | start == end = return ()
streamFile (RangeOnly start end) fp =
  let src = C.sourceIOHandle $ do
              handle <- openFile fp ReadMode
              hSeek handle AbsoluteSeek start
              return handle
      pull 0 = return ()
      pull rem = await >>= \case
        Nothing -> return ()
        Just bs ->
          let len = fromIntegral $ BS.length bs
          in  if len <= rem
                then yield bs >> pull (rem - len)
                else yield (BS.take (fromIntegral rem) bs)

  in src .| pull (end - start + 1)

