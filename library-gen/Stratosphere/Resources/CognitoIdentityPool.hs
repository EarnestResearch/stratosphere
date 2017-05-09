{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html

module Stratosphere.Resources.CognitoIdentityPool where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Data.Text

import Stratosphere.Values
import Stratosphere.ResourceProperties.CognitoIdentityPoolCognitoIdentityProvider
import Stratosphere.ResourceProperties.CognitoIdentityPoolCognitoStreams
import Stratosphere.ResourceProperties.CognitoIdentityPoolPushSync

-- | Full data type definition for CognitoIdentityPool. See
-- 'cognitoIdentityPool' for a more convenient constructor.
data CognitoIdentityPool =
  CognitoIdentityPool
  { _cognitoIdentityPoolAllowUnauthenticatedIdentities :: Val Bool'
  , _cognitoIdentityPoolCognitoEvents :: Maybe Object
  , _cognitoIdentityPoolCognitoIdentityProviders :: Maybe [CognitoIdentityPoolCognitoIdentityProvider]
  , _cognitoIdentityPoolCognitoStreams :: Maybe CognitoIdentityPoolCognitoStreams
  , _cognitoIdentityPoolDeveloperProviderName :: Maybe (Val Text)
  , _cognitoIdentityPoolIdentityPoolName :: Maybe (Val Text)
  , _cognitoIdentityPoolOpenIdConnectProviderARNs :: Maybe [Val Text]
  , _cognitoIdentityPoolPushSync :: Maybe CognitoIdentityPoolPushSync
  , _cognitoIdentityPoolSamlProviderARNs :: Maybe [Val Text]
  , _cognitoIdentityPoolSupportedLoginProviders :: Maybe Object
  } deriving (Show, Eq)

instance ToJSON CognitoIdentityPool where
  toJSON CognitoIdentityPool{..} =
    object $
    catMaybes
    [ Just ("AllowUnauthenticatedIdentities" .= _cognitoIdentityPoolAllowUnauthenticatedIdentities)
    , ("CognitoEvents" .=) <$> _cognitoIdentityPoolCognitoEvents
    , ("CognitoIdentityProviders" .=) <$> _cognitoIdentityPoolCognitoIdentityProviders
    , ("CognitoStreams" .=) <$> _cognitoIdentityPoolCognitoStreams
    , ("DeveloperProviderName" .=) <$> _cognitoIdentityPoolDeveloperProviderName
    , ("IdentityPoolName" .=) <$> _cognitoIdentityPoolIdentityPoolName
    , ("OpenIdConnectProviderARNs" .=) <$> _cognitoIdentityPoolOpenIdConnectProviderARNs
    , ("PushSync" .=) <$> _cognitoIdentityPoolPushSync
    , ("SamlProviderARNs" .=) <$> _cognitoIdentityPoolSamlProviderARNs
    , ("SupportedLoginProviders" .=) <$> _cognitoIdentityPoolSupportedLoginProviders
    ]

instance FromJSON CognitoIdentityPool where
  parseJSON (Object obj) =
    CognitoIdentityPool <$>
      obj .: "AllowUnauthenticatedIdentities" <*>
      obj .:? "CognitoEvents" <*>
      obj .:? "CognitoIdentityProviders" <*>
      obj .:? "CognitoStreams" <*>
      obj .:? "DeveloperProviderName" <*>
      obj .:? "IdentityPoolName" <*>
      obj .:? "OpenIdConnectProviderARNs" <*>
      obj .:? "PushSync" <*>
      obj .:? "SamlProviderARNs" <*>
      obj .:? "SupportedLoginProviders"
  parseJSON _ = mempty

-- | Constructor for 'CognitoIdentityPool' containing required fields as
-- arguments.
cognitoIdentityPool
  :: Val Bool' -- ^ 'cipAllowUnauthenticatedIdentities'
  -> CognitoIdentityPool
cognitoIdentityPool allowUnauthenticatedIdentitiesarg =
  CognitoIdentityPool
  { _cognitoIdentityPoolAllowUnauthenticatedIdentities = allowUnauthenticatedIdentitiesarg
  , _cognitoIdentityPoolCognitoEvents = Nothing
  , _cognitoIdentityPoolCognitoIdentityProviders = Nothing
  , _cognitoIdentityPoolCognitoStreams = Nothing
  , _cognitoIdentityPoolDeveloperProviderName = Nothing
  , _cognitoIdentityPoolIdentityPoolName = Nothing
  , _cognitoIdentityPoolOpenIdConnectProviderARNs = Nothing
  , _cognitoIdentityPoolPushSync = Nothing
  , _cognitoIdentityPoolSamlProviderARNs = Nothing
  , _cognitoIdentityPoolSupportedLoginProviders = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-allowunauthenticatedidentities
cipAllowUnauthenticatedIdentities :: Lens' CognitoIdentityPool (Val Bool')
cipAllowUnauthenticatedIdentities = lens _cognitoIdentityPoolAllowUnauthenticatedIdentities (\s a -> s { _cognitoIdentityPoolAllowUnauthenticatedIdentities = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-cognitoevents
cipCognitoEvents :: Lens' CognitoIdentityPool (Maybe Object)
cipCognitoEvents = lens _cognitoIdentityPoolCognitoEvents (\s a -> s { _cognitoIdentityPoolCognitoEvents = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-cognitoidentityproviders
cipCognitoIdentityProviders :: Lens' CognitoIdentityPool (Maybe [CognitoIdentityPoolCognitoIdentityProvider])
cipCognitoIdentityProviders = lens _cognitoIdentityPoolCognitoIdentityProviders (\s a -> s { _cognitoIdentityPoolCognitoIdentityProviders = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-cognitostreams
cipCognitoStreams :: Lens' CognitoIdentityPool (Maybe CognitoIdentityPoolCognitoStreams)
cipCognitoStreams = lens _cognitoIdentityPoolCognitoStreams (\s a -> s { _cognitoIdentityPoolCognitoStreams = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-developerprovidername
cipDeveloperProviderName :: Lens' CognitoIdentityPool (Maybe (Val Text))
cipDeveloperProviderName = lens _cognitoIdentityPoolDeveloperProviderName (\s a -> s { _cognitoIdentityPoolDeveloperProviderName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-identitypoolname
cipIdentityPoolName :: Lens' CognitoIdentityPool (Maybe (Val Text))
cipIdentityPoolName = lens _cognitoIdentityPoolIdentityPoolName (\s a -> s { _cognitoIdentityPoolIdentityPoolName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-openidconnectproviderarns
cipOpenIdConnectProviderARNs :: Lens' CognitoIdentityPool (Maybe [Val Text])
cipOpenIdConnectProviderARNs = lens _cognitoIdentityPoolOpenIdConnectProviderARNs (\s a -> s { _cognitoIdentityPoolOpenIdConnectProviderARNs = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-pushsync
cipPushSync :: Lens' CognitoIdentityPool (Maybe CognitoIdentityPoolPushSync)
cipPushSync = lens _cognitoIdentityPoolPushSync (\s a -> s { _cognitoIdentityPoolPushSync = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-samlproviderarns
cipSamlProviderARNs :: Lens' CognitoIdentityPool (Maybe [Val Text])
cipSamlProviderARNs = lens _cognitoIdentityPoolSamlProviderARNs (\s a -> s { _cognitoIdentityPoolSamlProviderARNs = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cognito-identitypool.html#cfn-cognito-identitypool-supportedloginproviders
cipSupportedLoginProviders :: Lens' CognitoIdentityPool (Maybe Object)
cipSupportedLoginProviders = lens _cognitoIdentityPoolSupportedLoginProviders (\s a -> s { _cognitoIdentityPoolSupportedLoginProviders = a })
