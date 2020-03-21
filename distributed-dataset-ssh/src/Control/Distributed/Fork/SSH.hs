{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StaticPointers #-}

module Control.Distributed.Fork.SSH where

import Control.Distributed.Fork
import Control.Distributed.Fork.Backend
import Control.Monad.STM
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Data.String.Interpolate.IsString
import qualified System.Process.Typed as P

sshBackend :: Backend
sshBackend = Backend $ \bs ->
  undefined

spawnSSH :: IO ()
spawnSSH = do
  hsMain <- mkHsMain
  let process =
        P.proc "ssh" ["utdemir.com", "sh", "-"]
          & P.setStdin
            ( P.byteStringInput . BL.fromStrict $
                closureScript
                  hsMain
                  (static (putStrLn "Hello from Haskell!"))
            )
          & P.setStdout P.byteStringOutput
          & P.setStderr P.inherit
  P.withProcessWait process $ \p ->
    print =<< atomically (P.getStdout p)

newtype HsMain = HsMain {hsMainToBase64 :: ByteString}

mkHsMain :: IO HsMain
mkHsMain = fmap (HsMain . B64.encode) $ BS.readFile =<< getExecutablePath

closureScript :: HsMain -> Closure (IO ()) -> ByteString
closureScript HsMain {hsMainToBase64} closure =
  [i|
set -o xtrace
set -o errexit
set -o pipefail

tmp="$(mktemp --suffix -dd)"
trap "rm -r '$tmp'" EXIT
cd "$tmp"

cat <<.EOF | base64 -d > "$tmp/hsmain"
#{hsMainToBase64}
.EOF
chmod +x "$tmp/hsmain"

cat <<.EOF | base64 -d | "$tmp/hsmain" "#{argExecutorMode}"
#{stdin}
.EOF
|]
  where
    stdin = B64.encode $ toBackendStdin (static Dict) closure
