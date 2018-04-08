{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StaticPointers #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

--------------------------------------------------------------------------------
import Universum
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Execute.LocalProcessBackend
import Network.Serverless.Batch
--------------------------------------------------------------------------------

s1 :: IO ()
s1 = do
  serverlessGuard

  let backend = localProcessBackend

  runAction backend $
    Read (error "") "localpath" &
    Map (static (* (2 :: Int))) &
    Write (error "") ""

  runAction backend $ do
    Read "somepath"
      >>= Map (static (* (2 :: Int)))
      >>= Write "otherpath"
