{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Control.Distributed.Fork.Lambda.Internal.Invoke
  ( withInvoke
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                                       as A
import           Data.Aeson.Lens
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Lazy                                  as BL
import           Data.ByteString.Base64                           as B64
import qualified Data.HashMap.Strict                              as HM
import qualified Data.Map.Strict                                  as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                        as T
import qualified Data.Text.Encoding                               as T
import           Network.AWS
import           Network.AWS.Lambda
import           Network.AWS.SQS
import           Text.Read
--------------------------------------------------------------------------------
import           Control.Concurrent.Throttled
import           Control.Distributed.Fork.Backend
import           Control.Distributed.Fork.Lambda.Internal.Stack (StackInfo (..))
--------------------------------------------------------------------------------

{-
Since we're going to get our answers asynchronously, we maintain a state with
callbacks for individual invocations.

Every individual invocation have an incrementing id, so we can distinguish
the responses.
-}
data LambdaState = LambdaState
  { lsInvocations :: M.Map Int (IO BS.ByteString -> IO ())
  , lsNextId      :: Int
  }

data LambdaEnv = LambdaEnv
  { leState :: MVar LambdaState
  , leStack :: StackInfo
  , leEnv   :: Env
  }

newLambdaEnv :: Env -> StackInfo -> IO LambdaEnv
newLambdaEnv env st  =
  LambdaEnv
    <$> newMVar (LambdaState M.empty 0)
    <*> return st
    <*> return env

{-
When invoking a function, we insert a new id to the state and then call Lambda.
-}
execute :: LambdaEnv -> Throttle -> BS.ByteString -> BackendM BS.ByteString
execute LambdaEnv{..} throttle input = do
  -- Modify environment
  mvar <- liftIO $ newEmptyMVar @(IO BS.ByteString)
  id' <- liftIO $ modifyMVar leState $ \LambdaState{..} -> return
    ( LambdaState { lsNextId = lsNextId + 1
                  , lsInvocations =
                      M.insert lsNextId (void . tryPutMVar mvar) lsInvocations
                  }
    , lsNextId
    )

  -- invoke the lambda function
  irs <- liftIO $ throttled throttle . runResourceT . runAWS leEnv $
    send $ invoke
      (siFunc leStack)
      (BL.toStrict . A.encode $ A.object
        [ (T.pack "d", A.toJSON . T.decodeUtf8 $ B64.encode input)
        , (T.pack "i", A.toJSON id')
        ])
      & iInvocationType ?~ Event
  submitted

  unless (irs ^. irsStatusCode `div` 100 == 2) $
    throwIO . InvokeException $
      "Invoke failed. Status code: " <> T.pack (show $ irs ^. irsStatusCode)

  -- wait fo the answer
  liftIO . join $ readMVar mvar


{-
And then we listen from answerQueue for the responses
-}
answerThread :: LambdaEnv -> IO ()
answerThread LambdaEnv {..} = runResourceT . runAWS leEnv . forever $ do
  msgs <- sqsReceiveSome $ siAnswerQueue leStack
  forM_ msgs $ \msg -> do
    id' <- liftIO $ decodeId msg
    liftIO . modifyMVar_ leState $ \s ->
      case M.updateLookupWithKey (\_ _ -> Nothing) id' (lsInvocations s) of
        (Nothing, _) -> return s
        (Just x, s') -> s { lsInvocations = s' } <$ x (decodeResponse msg)
  where
    decodeId :: Message -> IO Int
    decodeId msg =
      case HM.lookup "Id" (msg ^. mMessageAttributes) of
        Nothing -> throwIO . InvokeException $
          "Error decoding answer: can not find Id: " <> T.pack (show msg)
        Just av -> case readMaybe . T.unpack <$> av ^. mavStringValue of
          Nothing -> throwIO . InvokeException $
            "Error decoding answer: empty Id."
          Just Nothing -> throwIO . InvokeException $
            "Error decoding answer: can not decode Id."
          Just (Just x) -> return x

    decodeResponse :: Message -> IO BS.ByteString
    decodeResponse msg =
      case B64.decode . T.encodeUtf8 <$> msg ^. mBody of
        Nothing -> throwIO . InvokeException $
          "Error decoding answer: no body."
        Just (Left err) -> throwIO . InvokeException $
          "Error decoding answer: " <> T.pack err
        Just (Right x) -> return x

{-
And then we listen from answerQueue for the responses
-}
deadLetterThread :: LambdaEnv -> IO ()
deadLetterThread LambdaEnv {..} = runResourceT . runAWS leEnv . forever $ do
  msgs <- sqsReceiveSome $ siDeadLetterQueue leStack
  forM_ msgs $ \msg -> do
    id' <- liftIO $ decodeId msg
    liftIO . modifyMVar_ leState $ \s ->
      case M.updateLookupWithKey (\_ _ -> Nothing) id' (lsInvocations s) of
        (Nothing, _) -> return s
        (Just x, s') -> s { lsInvocations = s' } <$ x failure
  where
    failure :: IO a
    failure = throwIO . InvokeException $ "Lambda function failed."

    decodeId :: Message -> IO Int
    decodeId msg = case msg ^? mBody . _Just . key "i" . _Number of
      Nothing -> throwIO . InvokeException $
          "Can not find Id: " <> T.pack (show msg)
      Just x -> return $ truncate x

{-
A helper function to read from SQS queues.
-}
sqsReceiveSome :: T.Text -> AWS [Message]
sqsReceiveSome queue = do
  rmrs <- send $
    receiveMessage queue
      & rmVisibilityTimeout ?~ 10
      & rmWaitTimeSeconds ?~ 10
      & rmMaxNumberOfMessages ?~ 10
      & rmMessageAttributeNames .~ ["Id"]
  unless (rmrs ^. rmrsResponseStatus == 200) $
    liftIO . throwIO . InvokeException $
      "Error receiving messages: " <> T.pack (show $ rmrs ^. rmrsResponseStatus)
  let msgs = rmrs ^. rmrsMessages
  unless (null msgs) $ do
    dmbrs <- send $ deleteMessageBatch queue
      & dmbEntries  .~
        [ deleteMessageBatchRequestEntry
          (T.pack $ show i)
          (fromJust $ msg ^. mReceiptHandle)
        | (i, msg) <- zip [(0::Integer)..] msgs
        ]
    unless (dmbrs ^. dmbrsResponseStatus == 200) $
      liftIO . throwIO . InvokeException $
        "Error deleting received messages: " <> T.pack (show $ rmrs ^. rmrsResponseStatus)
  return msgs

--------------------------------------------------------------------------------

withInvoke :: Env -> StackInfo -> ((BS.ByteString -> BackendM BS.ByteString) -> IO a) -> IO a
withInvoke env stack f = do
  le <- newLambdaEnv env stack
  throttle <- newThrottle 128
  let answerT = async . forever $ catchAny (answerThread le) $ \ex -> print ex
      deadLetterT = async . forever $ catchAny (deadLetterThread le) $ \ex -> print ex
  threads <- (++) <$> replicateM 4 answerT <*> replicateM 2 deadLetterT
  f (execute le throttle) `finally` mapM_ cancel threads

--------------------------------------------------------------------------------

newtype InvokeException
  = InvokeException T.Text
  deriving Show

instance Exception InvokeException
