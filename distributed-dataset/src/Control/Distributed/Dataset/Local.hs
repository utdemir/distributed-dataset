{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StaticPointers #-}

module Control.Distributed.Dataset.Local
  ( withLocalTmpShuffleStore
  , -- * Re-exports
    localProcessBackend
  )
where

--------------------------------------------------------------------------------
import Conduit
import Control.Distributed.Closure
--------------------------------------------------------------------------------
import Control.Distributed.Dataset.ShuffleStore
import Control.Distributed.Fork.Local
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Conduit.Binary
import qualified Data.Conduit.Combinators as C
import System.Directory
import System.FilePath
import System.Posix.Temp
import Prelude hiding (rem)

--------------------------------------------------------------------------------

-- |
-- Uses a temporary directory in the local disk as a 'ShuffleStore'.
--
-- Useful for testing purposes.
withLocalTmpShuffleStore :: (ShuffleStore -> IO a) -> IO a
withLocalTmpShuffleStore act =
  bracket
    (mkdtemp "/tmp/dd-")
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
streamFile (RangeOnly start end) fp = sourceFileRange fp (Just start) (Just $ end - start + 1)
