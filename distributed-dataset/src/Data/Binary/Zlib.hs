module Data.Binary.Zlib where

import Codec.Compression.Zlib
import Data.Binary

newtype ZlibWrapper a = ZlibWrapper {unZlibWrapper :: a}

instance Binary a => Binary (ZlibWrapper a) where
  put = put . compress . encode . unZlibWrapper

  get = ZlibWrapper . decode . decompress <$> get
