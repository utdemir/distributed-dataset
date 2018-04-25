{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-
This module creates an environment for the Lambda function.
-}

module Network.Serverless.Execute.Lambda.Internal.Stack
  ( withStack
  , StackInfo (..)
  ) where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.Catch
import qualified Stratosphere                 as S
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value(Object))
import Lens.Micro
import Network.AWS.Waiter
import Network.AWS hiding (environment)
import Network.AWS.Lambda
import Network.AWS.CloudFormation
import Data.Aeson.QQ
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Lambda.Internal.Types
import Network.Serverless.Execute.Lambda.Internal.Constants
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
seTemplate :: S.Template
seTemplate =
  S.template
    (S.Resources
       [ S.resource "func" $
         S.LambdaFunctionProperties $
         S.lambdaFunction
           (S.lambdaFunctionCode
              & S.lfcS3Bucket ?~ S.Ref templateParameterS3Bucket
              & S.lfcS3Key ?~ S.Ref templateParameterS3Key)
           "handler.handle"
           (S.GetAtt "role" "Arn")
           (S.Literal S.Python27)
         & S.lfTimeout ?~ S.Literal 10
         & S.lfDeadLetterConfig ?~
             S.LambdaFunctionDeadLetterConfig (Just $ S.GetAtt "deadLetterQueue" "Arn")
       , S.resource "role" seRole
       , S.resource "answerQueue" $
         S.SQSQueueProperties $
         S.sqsQueue
       , S.resource "deadLetterQueue" $
         S.SQSQueueProperties $
         S.sqsQueue
       ]) &
  S.parameters ?~
  S.Parameters
    [ S.parameter templateParameterS3Bucket "String"
    , S.parameter templateParameterS3Key "String"
    ] &
  S.outputs ?~
  S.Outputs
    [ S.output templateOutputFunc (S.Ref "func")
    , S.output templateOutputAnswerQueue (S.Ref "answerQueue")
    , S.output templateOutputDeadLetterQueue (S.Ref "deadLetterQueue")
    ]

seRole :: S.ResourceProperties
seRole =
  S.IAMRoleProperties $
  S.iamRole assumeRolePolicy
  & S.iamrPolicies ?~
    [ S.iamRolePolicy sqsAccessPolicy (S.Literal "sqs-access")
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
        }]
      }
    |]
    valueToObject = \case
      Object hm -> hm
      _ -> error "invariant violation"


templateParameterS3Bucket :: T.Text
templateParameterS3Bucket = "s3bucket"

templateParameterS3Key :: T.Text
templateParameterS3Key = "s3key"

templateOutputFunc :: T.Text
templateOutputFunc = "output"

templateOutputAnswerQueue :: T.Text
templateOutputAnswerQueue = "answerQueue"

templateOutputDeadLetterQueue :: T.Text
templateOutputDeadLetterQueue = "deadLetterQueue"

--------------------------------------------------------------------------------

{-
Here we are actually creating the stack.

Tricky part is that, since our lambda function publish to answerQueue, it needs
to know the URL for it. However LambdaFunctionEnvironment in CloudFormation does
not support referring to stack variables inside, so we need to patch the function
afterwards to add the URL to the environment
-}
data StackInfo = StackInfo
  { siId :: T.Text
  , siFunc :: T.Text
  , siAnswerQueue :: T.Text
  , siDeadLetterQueue :: T.Text
  }

seCreateStack :: StackName -> S3Loc -> AWS StackInfo
seCreateStack (StackName stackName) (S3Loc (BucketName bucketName) path) = do
  csrs <-
    send $
      createStack stackName
        & csTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ S.encodeTemplate seTemplate)
        & csCapabilities .~ [CapabilityIAM]
        & csParameters .~
            [ parameter
                & pParameterKey ?~ templateParameterS3Bucket
                & pParameterValue ?~ bucketName
            , parameter
                & pParameterKey ?~ templateParameterS3Key
                & pParameterValue ?~ path
            ]
  unless (csrs ^. csrsResponseStatus == 200) $
    throwM $
     AWSException
       ("CloudFormation stack creation request failed." <> T.pack (show csrs))
  stackId <-
    case csrs ^. csrsStackId of
      Nothing ->
        throwM $
        AWSException
          "Could not determine stack id."
      Just xs -> return xs

  await stackCreateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return ()
    err ->
      throwM . AWSException $ "CloudFormation stack creation failed." <> T.pack (show err)

  dsrs <- send $ describeStacks & dStackName ?~ stackName
  unless (dsrs ^. dsrsResponseStatus == 200) $
    throwM . AWSException $
      "CloudFormation describeStack failed. Status code: "
      <> T.pack (show $ dsrs ^. dsrsResponseStatus)
  stackRs <- case dsrs ^. dsrsStacks of
    x:[] -> return x
    _ -> throwM $ AWSException "Unexpected answer from DescribeStacks."

  func <- case lookupOutput stackRs templateOutputFunc of
    Nothing -> throwM $ AWSException "Could not determine function name."
    Just t -> return t

  answerQueue <- case lookupOutput stackRs templateOutputAnswerQueue of
    Nothing -> throwM $ AWSException "Could not determine answerQueue URL."
    Just t -> return t

  deadLetterQueue <- case lookupOutput stackRs templateOutputDeadLetterQueue of
    Nothing -> throwM $ AWSException "Could not determine deadLetterQueue URL."
    Just t -> return t

  _ <- send $
    updateFunctionConfiguration func
      & ufcEnvironment ?~ (
         environment
           & eVariables ?~ HM.fromList [ (envAnswerQueueUrl, answerQueue) ]
        )

  return $ StackInfo { siId = stackId
                     , siFunc = func
                     , siAnswerQueue = answerQueue
                     , siDeadLetterQueue = deadLetterQueue
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

data AWSException
  = AWSException T.Text
  deriving Show

instance Exception AWSException

--------------------------------------------------------------------------------

withStack :: Env -> StackName -> S3Loc -> (StackInfo -> IO a) -> IO a
withStack env name loc f = do
  bracket create destroy f
  where
    create = runResourceT . runAWS env $ seCreateStack name loc
    destroy = runResourceT . runAWS env . seDeleteStack

