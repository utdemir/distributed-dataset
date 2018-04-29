{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}

module Network.AWS.Lambda.Invoke.Fixed where

--------------------------------------------------------------------------------
import Network.AWS.Lambda
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
--------------------------------------------------------------------------------

newtype FixedInvoke = FixedInvoke Invoke
  deriving (ToPath, ToQuery, ToHeaders, ToBody)

data FixedInvokeResponse
  = FixedInvokeResponse { _firsStatusCode :: !Int }

instance AWSRequest FixedInvoke where
  type Rs FixedInvoke = FixedInvokeResponse
  request = postBody lambda
  response
    = receiveBody
        (\ s _ _ ->
           return $ FixedInvokeResponse (fromEnum s))
