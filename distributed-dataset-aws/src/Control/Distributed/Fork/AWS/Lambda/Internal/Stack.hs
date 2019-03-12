{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
This module creates an environment for the Lambda function.
-}

module Control.Distributed.Fork.AWS.Lambda.Internal.Stack
  ( withStack
  , StackInfo (..)
  , awsUploadObject
  , awsObjectExists
  ) where

--------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Data.Aeson                                             (Value (Object))
import           Data.Aeson.QQ
import qualified Data.ByteString                                        as BS
import qualified Data.ByteString.Lazy                                   as BL
import qualified Data.HashMap.Strict                                    as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text                                              as T
import qualified Data.Text.Encoding                                     as T
import           Network.AWS                                            hiding (environment)
import           Network.AWS.CloudFormation
import           Network.AWS.Lambda
import qualified Network.AWS.S3                                         as S3
import           Network.AWS.Waiter
import qualified Stratosphere                                           as S
--------------------------------------------------------------------------------
import           Control.Distributed.Fork.AWS.Lambda.Internal.Constants
import           Control.Distributed.Fork.AWS.Lambda.Internal.Types
--------------------------------------------------------------------------------

{-
Our infrastructure in AWS Lambda looks like this.

We're using CloudFormation to create all the necessary services declaratively
and delete all of them by deleting the stack.

The components are:

* The lambda function.

* A role for the function:

    Lambda functions need to have a role attached. Currently we are both too
    lax (we're not restricting the sqs:SendMessage permission to relevant topics)
    and too strict (we're not letting user to provide a custom policy). This part
    is probably the first one to change.

* Two queues called answerQueue and deadLetterQueue

    Lambda functins have two ways to invoke them on-demand, synchronously and
    asynchronously. Using the synchronous invocation is simpler, you call the API
    and it returns the result. However it requires the HTTP connection to be open
    for the whole time. This puts a hard-cap on the scalability since the driver
    can only have so many open HTTP connections simultaneously.

    To mitigate this, we're going to use asynchronous invocation type, but use
    an SQS queue (answerQueue) to gather the results. We use another queue
    (deadLetterQueue) for collecting the failures.
-}
seTemplate :: StackOptions -> S.Template
seTemplate StackOptions{ soLambdaCode = S3Loc (BucketName bucketName) path, soLambdaMemory } =
  S.template
    (S.Resources
       [ S.resource templateOutputFunc $
         S.LambdaFunctionProperties $
         S.lambdaFunction
           (S.lambdaFunctionCode
              & S.lfcS3Bucket ?~ S.Literal bucketName
              & S.lfcS3Key ?~ S.Literal path)
           "handler.handle"
           (S.GetAtt "role" "Arn")
           (S.Literal S.Python27)
         & S.lfTimeout ?~ S.Literal 300
         & S.lfMemorySize ?~ S.Literal (fromIntegral soLambdaMemory)
         & S.lfDeadLetterConfig ?~
             S.LambdaFunctionDeadLetterConfig (Just $ S.GetAtt "deadLetterQueue" "Arn")
       , S.resource "role" seRole
       , S.resource templateOutputAnswerQueue $ S.SQSQueueProperties S.sqsQueue
       , S.resource templateOutputDeadLetterQueue $ S.SQSQueueProperties S.sqsQueue
       , S.resource templateOutputAnswerBucket $ S.S3BucketProperties S.s3Bucket
       ]) &
  S.templateOutputs ?~
    S.Outputs
      [ S.output templateOutputFunc (S.Ref templateOutputFunc)
      , S.output templateOutputAnswerQueue (S.Ref templateOutputAnswerQueue)
      , S.output templateOutputDeadLetterQueue (S.Ref templateOutputDeadLetterQueue)
      , S.output templateOutputAnswerBucket (S.Ref templateOutputAnswerBucket)
      ]

seRole :: S.ResourceProperties
seRole =
  S.IAMRoleProperties $
  S.iamRole assumeRolePolicy
  & S.iamrPolicies ?~
    [ S.iamRolePolicy sqsAccessPolicy (S.Literal "sqs")
    , S.iamRolePolicy cloudwatchPolicy (S.Literal "cloudwatch")
    ]
  where
    assumeRolePolicy = valueToObject [aesonQQ|
      {
        "Version":"2012-10-17",
        "Statement": [{
          "Effect": "Allow",
          "Principal": {
            "Service": [ "lambda.amazonaws.com" ]
          },
          "Action": [ "sts:AssumeRole" ]
        }]
      }
    |]
    sqsAccessPolicy = valueToObject [aesonQQ|
      {
        "Version": "2012-10-17",
        "Statement": [{
          "Effect": "Allow",
          "Action": [
              "sqs:SendMessage"
          ],
          "Resource": "arn:aws:sqs:*"
        }, {
          "Effect": "Allow",
          "Action": [
              "s3:PutObject",
              "s3:GetObject"
          ],
          "Resource": "arn:aws:s3:::*"
        }]
      }
    |]
    cloudwatchPolicy = valueToObject [aesonQQ|
      {
        "Version": "2012-10-17",
        "Statement": [{
          "Action": [
            "logs:CreateLogGroup",
            "logs:CreateLogStream",
            "logs:PutLogEvents"
          ],
          "Effect": "Allow",
          "Resource": "arn:aws:logs:*:*:*"
        }]
      }
    |]
    valueToObject = \case
      Object hm -> hm
      _ -> error "invariant violation"


templateOutputFunc :: T.Text
templateOutputFunc = "output"

templateOutputAnswerQueue :: T.Text
templateOutputAnswerQueue = "answerQueue"

templateOutputDeadLetterQueue :: T.Text
templateOutputDeadLetterQueue = "deadLetterQueue"

templateOutputAnswerBucket :: T.Text
templateOutputAnswerBucket = "answerBucket"

--------------------------------------------------------------------------------

{-
Before creating a stack, we need to upload the artifact to S3.
-}
awsUploadObject :: S3Loc -> BS.ByteString -> AWS ()
awsUploadObject (S3Loc (BucketName bucket) path) contents = do
  pors <- send $ S3.putObject
                   (S3.BucketName bucket)
                   (S3.ObjectKey path)
                   (toBody contents)
  unless (pors ^. S3.porsResponseStatus == 200) $
    throwM . StackException $
      "Upload failed. Status code: " <> T.pack (show $ pors ^. S3.porsResponseStatus)

awsObjectExists :: S3Loc -> AWS Bool
awsObjectExists (S3Loc (BucketName bucket) path) = do
  lors <- send $
    S3.listObjects (S3.BucketName bucket)
      & S3.loPrefix ?~ path
  unless (lors ^. S3.lorsResponseStatus == 200) $
    throwM . StackException $
      "List objects failed. Status code: " <> T.pack (show lors)
  let files = map (view S3.oKey) (lors ^. S3.lorsContents)
  return $ any (\(S3.ObjectKey k) -> k == path) files

--------------------------------------------------------------------------------

{-
Here we are actually creating the stack.

Tricky part is that, since our lambda function publish to answerQueue, it needs
to know the URL for it. However LambdaFunctionEnvironment in CloudFormation does
not support referring to stack variables inside, so we need to patch the function
afterwards to add the URL to the environment
-}
data StackInfo = StackInfo
  { siId              :: T.Text
  , siFunc            :: T.Text
  , siAnswerQueue     :: T.Text
  , siDeadLetterQueue :: T.Text
  , siAnswerBucket    :: T.Text
  }

seCreateStack :: StackOptions -> AWS StackInfo
seCreateStack options@StackOptions { soName = StackName stackName } = do
  csrs <-
    send $
      createStack stackName
        & csTemplateBody ?~
            (T.decodeUtf8 . BL.toStrict . S.encodeTemplate $ seTemplate options)
        & csCapabilities .~ [CapabilityIAM]
  unless (csrs ^. csrsResponseStatus == 200) $
    throwM $
     StackException
       ("CloudFormation stack creation request failed." <> T.pack (show csrs))
  stackId <-
    case csrs ^. csrsStackId of
      Nothing ->
        throwM $
        StackException
          "Could not determine stack id."
      Just xs -> return xs

  await stackCreateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return ()
    err ->
      throwM . StackException $ "CloudFormation stack creation failed." <> T.pack (show err)

  dsrs <- send $ describeStacks & dStackName ?~ stackName
  unless (dsrs ^. dsrsResponseStatus == 200) $
    throwM . StackException $
      "CloudFormation describeStack failed. Status code: "
      <> T.pack (show $ dsrs ^. dsrsResponseStatus)
  stackRs <- case dsrs ^. dsrsStacks of
    [x] -> return x
    _   -> throwM $ StackException "Unexpected answer from DescribeStacks."

  func <- case lookupOutput stackRs templateOutputFunc of
    Nothing -> throwM $ StackException "Could not determine function name."
    Just t  -> return t

  answerQueue <- case lookupOutput stackRs templateOutputAnswerQueue of
    Nothing -> throwM $ StackException "Could not determine answerQueue URL."
    Just t  -> return t

  deadLetterQueue <- case lookupOutput stackRs templateOutputDeadLetterQueue of
    Nothing -> throwM $ StackException "Could not determine deadLetterQueue URL."
    Just t -> return t

  answerBucket <- case lookupOutput stackRs templateOutputAnswerBucket of
    Nothing -> throwM $ StackException "Could not determine answerBucket URL."
    Just t  -> return t

  _ <- send $
    updateFunctionConfiguration func
      & ufcEnvironment ?~ (
         environment
           & eVariables ?~ HM.fromList [ (envAnswerQueueUrl, answerQueue)
                                       , (envAnswerBucketUrl, answerBucket)
                                       ]
        )

  return $ StackInfo { siId = stackId
                     , siFunc = func
                     , siAnswerQueue = answerQueue
                     , siDeadLetterQueue = deadLetterQueue
                     , siAnswerBucket = answerBucket
                     }

  where
    lookupOutput :: Stack -> T.Text -> Maybe T.Text
    lookupOutput st key =
      fmap (\i -> fromJust $ i ^. oOutputValue) .
      find (\o -> o ^. oOutputKey == Just key) $
      st ^. sOutputs

seDeleteStack :: StackInfo -> AWS ()
seDeleteStack = void . send . deleteStack . siId

--------------------------------------------------------------------------------

newtype StackException
  = StackException T.Text
  deriving Show

instance Exception StackException

--------------------------------------------------------------------------------

withStack :: StackOptions -> Env -> (StackInfo -> IO a) -> IO a
withStack opts env = bracket create destroy
  where
    create =
      runResourceT . runAWS env $ seCreateStack opts
    destroy =
      unless (soKeep opts) . runResourceT . runAWS env . seDeleteStack
