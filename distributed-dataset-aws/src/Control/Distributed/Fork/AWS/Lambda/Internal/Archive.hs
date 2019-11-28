{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This module contains the executables for the Lambda function.
-}
module Control.Distributed.Fork.AWS.Lambda.Internal.Archive
  ( Archive (..),
    mkArchive,
    archiveSize,
    archiveChecksum,
  )
where

--------------------------------------------------------------------------------
import Codec.Archive.Zip hiding (Archive)
--------------------------------------------------------------------------------
import Control.Distributed.Fork.AWS.Lambda.Internal.Constants
import Control.Distributed.Fork.Backend
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA
import Data.Elf
import Data.Function
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------

{-
Since AWS Lambda does not support binary execution by default, our entry point
is a small Python script, whose only purpose is to execute the attached Haskell
binary, provide the input from standard input and return the standard output to
the queue.
-}
handlerPy :: BS.ByteString
handlerPy =
  T.encodeUtf8 $
    T.pack
      [i|
import os
import subprocess
from uuid import uuid4
from json import dumps
from base64 import b64encode, b64decode

import boto3

queue_url = os.environ["#{envAnswerQueueUrl}"]
bucket_url = os.environ["#{envAnswerBucketUrl}"]

sqs = boto3.client('sqs')
s3 = boto3.client('s3')

def send_message(body):
    sqs.send_message(
      QueueUrl=queue_url,
      MessageBody=dumps(body),
    )

def handle(event, context):
    id = event["i"]

    popen = subprocess.Popen(
       ["./#{hsMainName}", "#{argExecutorMode}"],
       stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (out, _) = popen.communicate(b64decode(event["d"]))

    if popen.returncode:
      print "Subprocess failed, code: ", popen.returncode
      exit(1)

    if len(out) < 200000:
      send_message({
        "id": id,
        "type": "response-inline",
        "payload": b64encode(out)
      })
    else:
      fname = str(uuid4())
      s3.put_object(
        Bucket = bucket_url,
        Key = fname,
        Body = out
      )
      send_message({
        "id": id,
        "type": "response-s3",
        "path": fname
      })
|]

{-
And we read the current executable.

Since it'll run on AWS Lambda, it needs to be a statically linked Linux
executable, so we do a preliminary check here.
-}
mkHsMain :: IO BS.ByteString
mkHsMain = do
  path <- getExecutablePath
  contents <- BS.readFile path
  assertBinary contents
  return contents

assertBinary :: BS.ByteString -> IO ()
assertBinary contents = do
  elf <-
    (return $! parseElf contents)
      `catch` (\(_ :: SomeException) -> throwIO FileExceptionNotElf)
  unless (elfClass elf == ELFCLASS64) $
    throwIO FileExceptionNot64Bit
  when (any (\s -> elfSegmentType s == PT_DYNAMIC) (elfSegments elf)) $
    throwIO FileExceptionNotStatic

data FileException
  = FileExceptionNotElf
  | FileExceptionNot64Bit
  | FileExceptionNotStatic

instance Exception FileException

instance Show FileException where
  show FileExceptionNotElf =
    [i|
    Error: I am not an ELF (Linux) binary.

    The executable will run on AWS environment, because of that
    this library currently only supports Linux.
    |]
  show FileExceptionNot64Bit =
    [i|
    Error: I am not a 64bit executable.

    AWS Lambda currently only runs 64 bit executables.
    |]
  show FileExceptionNotStatic =
    [i|
    Error: I am not a dynamic executable.

    Since the executable will run on AWS environment, it needs
    to be statically linked.

    You can give GHC "-optl-static -optl-pthread -fPIC" flags
    to statically compile executables.
    |]

{-
And we're going to put all of them in a zip archive.
-}
newtype Archive
  = Archive {archiveToByteString :: BS.ByteString}

mkArchive :: IO Archive
mkArchive = do
  hsMain <- mkHsMain
  return . Archive . BL.toStrict . fromArchive $
    emptyArchive
      & addEntryToArchive
        (toEntry handlerPyName 0 $ BL.fromStrict handlerPy)
      & addEntryToArchive
        (toEntry hsMainName 0 $ BL.fromStrict hsMain)
          { eExternalFileAttributes = 0b10000 {- rwx -}
          }

archiveSize :: Archive -> Integer
archiveSize = fromIntegral . BS.length . archiveToByteString

archiveChecksum :: Archive -> T.Text
archiveChecksum =
  T.pack . showDigest . sha1 . BL.fromStrict . archiveToByteString
