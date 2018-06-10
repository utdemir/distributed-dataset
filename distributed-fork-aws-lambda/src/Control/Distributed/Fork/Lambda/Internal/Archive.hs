{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This module contains the executables for the Lambda function.
-}

module Control.Distributed.Fork.Lambda.Internal.Archive
  ( Archive (..)
  , mkArchive
  , archiveSize
  , archiveChecksum
  ) where

--------------------------------------------------------------------------------
import           Codec.Archive.Zip                                  hiding
                                                                     (Archive)
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString                                    as BS
import qualified Data.ByteString.Lazy                               as BL
import           Data.Digest.Pure.SHA
import           Data.Elf
import           Data.Function
import           Data.String.Interpolate
import qualified Data.Text                                          as T
import qualified Data.Text.Encoding                                 as T
--------------------------------------------------------------------------------
import           Control.Distributed.Fork.Backend
import           Control.Distributed.Fork.Lambda.Internal.Constants
--------------------------------------------------------------------------------

{-
Since AWS Lambda does not support binary execution by default, our entry point
is a small Python script, whose only purpose is to execute the attached Haskell
binary, provide the input from standard input and return the standard output to
the queue.
-}
handlerPy :: BS.ByteString
handlerPy = T.encodeUtf8 $ T.pack [i|
import os
import subprocess
from base64 import *
import boto3

queue_url = os.environ["#{envAnswerQueueUrl}"]
client = boto3.client('sqs')

def handle(event, context):
    popen = subprocess.Popen(
       ["./#{hsMainName}", "#{argExecutorMode}"],
       stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    (out, _) = popen.communicate(b64decode(event["d"]))
    client.send_message(
      QueueUrl=queue_url,
      MessageBody=b64encode(out),
      MessageAttributes={
        "Id": {
            "DataType": "Number",
            "StringValue": str(event["i"])
        }
      }
    )

|]

{-
And we read the current executable.

Since it'll run on AWS Lambda, it needs to be a statically linked Linux
executable, so we do a preliminary check here. We're calling the "file"
command here instead of using libmagic, because trying to statically compile
it caused problems on my system.
-}
mkHsMain :: IO BS.ByteString
mkHsMain = do
  path <- getExecutablePath
  contents <- BS.readFile path

  elf <- return (parseElf contents)
    `catch` (\(_ :: SomeException) -> throwIO FileExceptionNotElf)
  unless (elfClass elf == ELFCLASS64) $
    throwIO FileExceptionNot64Bit
  when (PT_DYNAMIC `elem` map elfSegmentType (elfSegments elf)) $
    throwIO FileExceptionNotStatic

  return contents

data FileException
  = FileExceptionNotElf
  | FileExceptionNot64Bit
  | FileExceptionNotStatic

instance Exception FileException

instance Show FileException where
  show FileExceptionNotElf = [i|
    Error: I am not an ELF binary.

    The executable will run on AWS environment, because of that
    this library currently only supports Linux.
    |]
  show FileExceptionNot64Bit = [i|
    Error: I am not a 64bit executable.

    AWS Lambda currently only runs 64 bit executables.
    |]
  show FileExceptionNotStatic = [i|
    Error: I am not a dynamic executable.

    Since the executable will run on AWS environment, it needs
    to be statically linked.

    You can give GHC "-optl-static -optl-pthread -fPIC" flags
    to statically compile executables.
    |]

{-
And we're going to put all of them in a zip archive.
-}
newtype Archive =
  Archive { archiveToByteString :: BS.ByteString }

mkArchive :: IO Archive
mkArchive = do
  hsMain <- mkHsMain
  return . Archive . BL.toStrict . fromArchive $
    emptyArchive
      & addEntryToArchive
          (toEntry handlerPyName 0 $ BL.fromStrict handlerPy)
      & addEntryToArchive
          (toEntry hsMainName 0 $ BL.fromStrict hsMain)
            { eExternalFileAttributes = 0b10000 {- rwx -} }

archiveSize :: Archive -> Integer
archiveSize = fromIntegral . BS.length . archiveToByteString

archiveChecksum :: Archive -> T.Text
archiveChecksum =
  T.pack . showDigest . sha1 . BL.fromStrict . archiveToByteString
