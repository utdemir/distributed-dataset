{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.Serverless.Execute.Lambda.Deploy
  ( withStack
  , awsUploadObject
  , awsInvoke
  , DeployException (AWSError)
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
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Aeson (object, Value(Array))
import Lens.Micro
import Network.AWS.Waiter
import Network.AWS
import qualified Network.AWS.S3 as S3
import Network.AWS.Lambda
import Network.AWS.CloudFormation
--------------------------------------------------------------------------------
import Network.Serverless.Execute.Lambda.Types
--------------------------------------------------------------------------------

parameterS3Bucket :: T.Text
parameterS3Bucket = "s3bucket"

parameterS3Key :: T.Text
parameterS3Key = "s3key"

--------------------------------------------------------------------------------

data DeployException
  = InvalidFile FilePath T.Text
  | AWSError T.Text T.Text
  deriving Show

instance Exception DeployException where
  displayException (InvalidFile fpath desc) =
    T.unpack $ "Error reading " <> T.pack fpath <> ": " <> desc
  displayException (AWSError hdr desc) = T.unpack $ hdr <> ": " <> desc

--------------------------------------------------------------------------------

withStack :: Env -> StackName -> S3Loc -> (LambdaId -> IO a) -> IO a
withStack env name loc f = do
  bracket create destroy go
  where
    create = runResourceT . runAWS env $ awsCreateStack name loc initialTemplate
    destroy = runResourceT . runAWS env . awsDeleteStack
    go sid = do
      st <- runResourceT . runAWS env $ awsDescribeStack sid
      lid <-
        case st >>= getLambdaId of
          Nothing -> throwM $ AWSError "Couldn't determine LambdaFunction id" ""
          Just l -> return l
      f lid


--------------------------------------------------------------------------------

getLambdaId :: Stack -> Maybe LambdaId
getLambdaId st =
  fmap (\i -> LambdaId $ fromJust $ i ^. oOutputValue) .
  find (\o -> o ^. oOutputKey == Just "func") $
  st ^. sOutputs

--------------------------------------------------------------------------------

awsCreateStack :: StackName
               -> S3Loc
               -> S.Template
               -> AWS StackId
awsCreateStack (StackName stackName) (S3Loc (BucketName bucketName) path) tpl = do
  csrs <-
    send $
      createStack stackName
        & csTemplateBody ?~
            (T.decodeUtf8 . BL.toStrict $ S.encodeTemplate tpl)
        & csCapabilities .~ [CapabilityIAM]
        & csParameters .~
            [ parameter
                & pParameterKey ?~ parameterS3Bucket
                & pParameterValue ?~ bucketName
            , parameter
                & pParameterKey ?~ parameterS3Key
                & pParameterValue ?~ path
            ]
  unless (csrs ^. csrsResponseStatus == 200) $
    throwM $
     AWSError
       "CloudFormation stack creation failed."
        ("Status code: " <> T.pack (show $ csrs ^. csrsResponseStatus))
  stackId <-
    case csrs ^. csrsStackId of
      Nothing ->
        throwM $
        AWSError
          "CloudFormation stack creation failed."
          "Could not determine stack id."
      Just xs -> return xs
  await stackCreateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return $ StackId stackId
    err ->
      throwM $
      AWSError "CloudFormation stack creation failed." (T.pack $ show err)

awsDescribeStack :: StackId -> AWS (Maybe Stack)
awsDescribeStack (StackId name) = do
  dsrs <- send $ describeStacks & dStackName ?~ name
  unless (dsrs ^. dsrsResponseStatus == 200) $
    throwM $
    AWSError
      "CloudFormation describeStack failed."
      ("Status code: " <> T.pack (show $ dsrs ^. dsrsResponseStatus))
  case dsrs ^. dsrsStacks of
    [] -> return Nothing
    x:[] -> return (Just x)
    _ -> throwM $ AWSError "Unexpected answer from DescribeStacks." ""

awsDeleteStack :: StackId -> AWS ()
awsDeleteStack (StackId stackId) = void $ send (deleteStack stackId)

awsUploadObject :: S3Loc -> BL.ByteString -> AWS ()
awsUploadObject (S3Loc (BucketName bucketId) path) contents = do
  pors <- send $ S3.putObject
                   (S3.BucketName bucketId)
                   (S3.ObjectKey path)
                   (toBody contents)
  unless (pors ^. S3.porsResponseStatus == 200) $
    throwM $ AWSError "Upload failed."
                      ("Status code: " <> T.pack (show $ pors ^. S3.porsResponseStatus))

  return ()

awsInvoke :: LambdaId -> HM.HashMap T.Text Value -> AWS (HM.HashMap T.Text Value)
awsInvoke (LambdaId lambdaId) params = do
  irs <- send $ invoke
                   lambdaId
                   params
  unless (irs ^. irsStatusCode == 200) $
    throwM $ AWSError "Invoke failed."
                      ("Status code: " <> T.pack (show $ irs ^. irsStatusCode))

  return . fromJust $ irs ^. irsPayload

--------------------------------------------------------------------------------

initialTemplate :: S.Template
initialTemplate =
  S.template
    (S.Resources
       [ S.resource "role" defaultRole
       , S.resource "func" $
         S.LambdaFunctionProperties $
         S.lambdaFunction
           (S.lambdaFunctionCode & S.lfcS3Bucket ?~ S.Ref parameterS3Bucket &
            S.lfcS3Key ?~
            S.Ref parameterS3Key)
           "handler.handle"
           (S.GetAtt "role" "Arn")
           (S.Literal S.Python27) &
         S.lfTimeout ?~
         S.Literal 10
       ]) &
  S.templateParameters ?~
  S.Parameters
    [ S.parameter parameterS3Bucket "String"
    , S.parameter parameterS3Key "String"
    ] &
  S.templateOutputs ?~
  S.Outputs
    [ S.output "func" (S.Ref "func")
    ]

defaultRole :: S.ResourceProperties
defaultRole =
  S.IAMRoleProperties $
  S.iamRole $
  HM.fromList
    [ ("Version", "2012-10-17")
    , ( "Statement"
      , object
          [ ("Effect", "Allow")
          , ( "Principal"
            , object [("Service", Array $ V.fromList ["lambda.amazonaws.com"])])
          , ("Action", Array $ V.fromList ["sts:AssumeRole"])
          ])
    ]
