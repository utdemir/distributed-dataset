{-# LANGUAGE ScopedTypeVariables #-}
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
import           Conduit
import qualified Data.Aeson                                       as A
import           Data.Aeson.Lens
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Lazy                                  as BL
import           Data.ByteString.Base64                           as B64
import qualified Data.Map.Strict                                  as M
import           Data.Maybe
import qualified Data.Text                                        as T
import qualified Data.Text.Encoding                               as T
import           Control.Monad.Trans.AWS (sinkBody)
import           Network.AWS
import           Network.AWS.Lambda
import           Network.AWS.SQS
import           Network.AWS.S3 as S3
--------------------------------------------------------------------------------
import           Control.Concurrent.Throttled
import           Control.Distributed.Fork.Backend
import           Control.Distributed.Fork.Lambda.Internal.Types
import           Control.Distributed.Fork.Lambda.Internal.Stack (StackInfo (..))
--------------------------------------------------------------------------------

{-
Since we're going to get our answers asynchronously, we maintain a state with
callbacks for individual invocations.

Every individual invocation have an incrementing id, so we can distinguish
the responses.
-}
data LambdaState = LambdaState
  { lsInvocations :: M.Map Int (IO ResponsePayload -> IO ())
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
execute :: LambdaEnv
        -> (Throttle, Throttle, Throttle)
        -> BS.ByteString
        -> BackendM BS.ByteString
execute LambdaEnv{..} (invocationThrottle, executionThrottle, downloadThrottle) input = do
  -- Modify environment
  mvar <- liftIO $ newEmptyMVar @(IO ResponsePayload)
  id' <- liftIO $ modifyMVar leState $ \LambdaState{..} -> return
    ( LambdaState { lsNextId = lsNextId + 1
                  , lsInvocations =
                      M.insert lsNextId (void . tryPutMVar mvar) lsInvocations
                  }
    , lsNextId
    )

  answer <- throttled executionThrottle $ do
    -- invoke the lambda function
    irs <- throttled invocationThrottle $ runResourceT . runAWS leEnv $
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

  case answer of
    ResponsePayloadInline p ->
      case B64.decode . T.encodeUtf8 $ p of
        Left err -> throwIO . InvokeException $
          "Error decoding answer: " <> T.pack err
        Right x -> return x
    ResponsePayloadS3 p ->
      throttled downloadThrottle $
        runResourceT . runAWS leEnv $ do
          let bucketName = S3.BucketName $ siAnswerBucket leStack
          gors <- send $ getObject bucketName (ObjectKey p)
          unless (gors ^. gorsResponseStatus `div` 100 == 2) $
            throwIO . InvokeException $
            "Downloading result failed. Status code: " <> T.pack (show $ gors ^. gorsResponseStatus)
          bs <- mconcat <$> sinkBody (gors ^. gorsBody) sinkList
          dors <- send $ deleteObject bucketName (ObjectKey p)
          unless (dors ^. dorsResponseStatus `div` 100 == 2) $
            throwIO . InvokeException $
            "Deleting result failed. Status code: " <> T.pack (show $ dors ^. dorsResponseStatus)
          return bs

{-
And then we listen from answerQueue for the responses
-}
answerThread :: LambdaEnv -> IO ()
answerThread LambdaEnv {..} = runResourceT . runAWS leEnv . forever $ do
  msgs <- sqsReceiveSome $ siAnswerQueue leStack
  forM_ msgs $ \msg -> do
    Response id' payload <- case A.decodeStrict . T.encodeUtf8 <$> msg ^. mBody of
      Nothing -> throwIO . InvokeException $
          "Error decoding answer: no body."
      Just Nothing -> throwIO . InvokeException $
          "Error decoding answer: invalid json: " <> T.pack (show msg)
      Just (Just r) -> return r

    liftIO . modifyMVar_ leState $ \s ->
      case M.updateLookupWithKey (\_ _ -> Nothing) id' (lsInvocations s) of
        (Nothing, _) -> return s
        (Just x, s') -> s { lsInvocations = s' } <$ x (return payload)

{-
https://docs.aws.amazon.com/lambda/latest/dg/dlq.html
-}
deadLetterThread :: LambdaEnv -> IO ()
deadLetterThread LambdaEnv {..} = runResourceT . runAWS leEnv . forever $ do
  msgs <- sqsReceiveSome $ siDeadLetterQueue leStack
  forM_ msgs $ \msg -> do
    id' <- liftIO $ decodeId msg
    liftIO . modifyMVar_ leState $ \s ->
      case M.updateLookupWithKey (\_ _ -> Nothing) id' (lsInvocations s) of
        (Nothing, _) -> return s
        (Just x, s') -> do
          let errMsg =
                msg
                  ^. mMessageAttributes
                  . at "ErrorMessage"
                  . _Just
                  . mavStringValue
          x . throwIO . InvokeException $
            "Lambda function failed: " <> fromMaybe "error message not found." errMsg
          return $ s { lsInvocations = s' }
  where
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
      & rmMessageAttributeNames .~ ["All"]
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

withInvoke :: Env
           -> (Throttle, Throttle, Throttle)
           -> StackInfo
           -> ((BS.ByteString -> BackendM BS.ByteString) -> IO a)
           -> IO a
withInvoke env throttles stack f = do
  le <- newLambdaEnv env stack
  let answerT = async . forever $ catchAny (answerThread le) $ \ex -> print ex
      deadLetterT = async . forever $ catchAny (deadLetterThread le) $ \ex -> print ex
  threads <- (++) <$> replicateM 4 answerT <*> replicateM 2 deadLetterT
  f (execute le throttles) `finally` mapM_ cancel threads

--------------------------------------------------------------------------------

newtype InvokeException
  = InvokeException T.Text
  deriving Show

instance Exception InvokeException
