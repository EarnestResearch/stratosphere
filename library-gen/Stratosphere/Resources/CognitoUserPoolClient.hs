{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html

module Stratosphere.Resources.CognitoUserPoolClient where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Data.Text

import Stratosphere.Values


-- | Full data type definition for CognitoUserPoolClient. See
-- 'cognitoUserPoolClient' for a more convenient constructor.
data CognitoUserPoolClient =
  CognitoUserPoolClient
  { _cognitoUserPoolClientClientName :: Maybe (Val Text)
  , _cognitoUserPoolClientExplicitAuthFlows :: Maybe [Val Text]
  , _cognitoUserPoolClientGenerateSecret :: Maybe (Val Bool')
  , _cognitoUserPoolClientReadAttributes :: Maybe [Val Text]
  , _cognitoUserPoolClientRefreshTokenValidity :: Maybe (Val Double')
  , _cognitoUserPoolClientUserPoolId :: Val Text
  , _cognitoUserPoolClientWriteAttributes :: Maybe [Val Text]
  } deriving (Show, Eq)

instance ToJSON CognitoUserPoolClient where
  toJSON CognitoUserPoolClient{..} =
    object $
    catMaybes
    [ ("ClientName" .=) <$> _cognitoUserPoolClientClientName
    , ("ExplicitAuthFlows" .=) <$> _cognitoUserPoolClientExplicitAuthFlows
    , ("GenerateSecret" .=) <$> _cognitoUserPoolClientGenerateSecret
    , ("ReadAttributes" .=) <$> _cognitoUserPoolClientReadAttributes
    , ("RefreshTokenValidity" .=) <$> _cognitoUserPoolClientRefreshTokenValidity
    , Just ("UserPoolId" .= _cognitoUserPoolClientUserPoolId)
    , ("WriteAttributes" .=) <$> _cognitoUserPoolClientWriteAttributes
    ]

instance FromJSON CognitoUserPoolClient where
  parseJSON (Object obj) =
    CognitoUserPoolClient <$>
      obj .:? "ClientName" <*>
      obj .:? "ExplicitAuthFlows" <*>
      obj .:? "GenerateSecret" <*>
      obj .:? "ReadAttributes" <*>
      obj .:? "RefreshTokenValidity" <*>
      obj .: "UserPoolId" <*>
      obj .:? "WriteAttributes"
  parseJSON _ = mempty

-- | Constructor for 'CognitoUserPoolClient' containing required fields as
-- arguments.
cognitoUserPoolClient
  :: Val Text -- ^ 'cupcUserPoolId'
  -> CognitoUserPoolClient
cognitoUserPoolClient userPoolIdarg =
  CognitoUserPoolClient
  { _cognitoUserPoolClientClientName = Nothing
  , _cognitoUserPoolClientExplicitAuthFlows = Nothing
  , _cognitoUserPoolClientGenerateSecret = Nothing
  , _cognitoUserPoolClientReadAttributes = Nothing
  , _cognitoUserPoolClientRefreshTokenValidity = Nothing
  , _cognitoUserPoolClientUserPoolId = userPoolIdarg
  , _cognitoUserPoolClientWriteAttributes = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-clientname
cupcClientName :: Lens' CognitoUserPoolClient (Maybe (Val Text))
cupcClientName = lens _cognitoUserPoolClientClientName (\s a -> s { _cognitoUserPoolClientClientName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-explicitauthflows
cupcExplicitAuthFlows :: Lens' CognitoUserPoolClient (Maybe [Val Text])
cupcExplicitAuthFlows = lens _cognitoUserPoolClientExplicitAuthFlows (\s a -> s { _cognitoUserPoolClientExplicitAuthFlows = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-generatesecret
cupcGenerateSecret :: Lens' CognitoUserPoolClient (Maybe (Val Bool'))
cupcGenerateSecret = lens _cognitoUserPoolClientGenerateSecret (\s a -> s { _cognitoUserPoolClientGenerateSecret = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-readattributes
cupcReadAttributes :: Lens' CognitoUserPoolClient (Maybe [Val Text])
cupcReadAttributes = lens _cognitoUserPoolClientReadAttributes (\s a -> s { _cognitoUserPoolClientReadAttributes = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-refreshtokenvalidity
cupcRefreshTokenValidity :: Lens' CognitoUserPoolClient (Maybe (Val Double'))
cupcRefreshTokenValidity = lens _cognitoUserPoolClientRefreshTokenValidity (\s a -> s { _cognitoUserPoolClientRefreshTokenValidity = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-userpoolid
cupcUserPoolId :: Lens' CognitoUserPoolClient (Val Text)
cupcUserPoolId = lens _cognitoUserPoolClientUserPoolId (\s a -> s { _cognitoUserPoolClientUserPoolId = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-userpoolclient.html#cfn-cognito-userpoolclient-writeattributes
cupcWriteAttributes :: Lens' CognitoUserPoolClient (Maybe [Val Text])
cupcWriteAttributes = lens _cognitoUserPoolClientWriteAttributes (\s a -> s { _cognitoUserPoolClientWriteAttributes = a })