{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-vpcendpointservice.html

module Stratosphere.Resources.EC2VPCEndpointService where

import Stratosphere.ResourceImports


-- | Full data type definition for EC2VPCEndpointService. See
-- 'ec2VPCEndpointService' for a more convenient constructor.
data EC2VPCEndpointService =
  EC2VPCEndpointService
  { _eC2VPCEndpointServiceAcceptanceRequired :: Maybe (Val Bool)
  , _eC2VPCEndpointServiceNetworkLoadBalancerArns :: ValList Text
  } deriving (Show, Eq)

instance ToJSON EC2VPCEndpointService where
  toJSON EC2VPCEndpointService{..} =
    object $
    catMaybes
    [ fmap (("AcceptanceRequired",) . toJSON . fmap Bool') _eC2VPCEndpointServiceAcceptanceRequired
    , (Just . ("NetworkLoadBalancerArns",) . toJSON) _eC2VPCEndpointServiceNetworkLoadBalancerArns
    ]

instance FromJSON EC2VPCEndpointService where
  parseJSON (Object obj) =
    EC2VPCEndpointService <$>
      fmap (fmap (fmap unBool')) (obj .:? "AcceptanceRequired") <*>
      (obj .: "NetworkLoadBalancerArns")
  parseJSON _ = mempty

-- | Constructor for 'EC2VPCEndpointService' containing required fields as
-- arguments.
ec2VPCEndpointService
  :: ValList Text -- ^ 'ecvpcesNetworkLoadBalancerArns'
  -> EC2VPCEndpointService
ec2VPCEndpointService networkLoadBalancerArnsarg =
  EC2VPCEndpointService
  { _eC2VPCEndpointServiceAcceptanceRequired = Nothing
  , _eC2VPCEndpointServiceNetworkLoadBalancerArns = networkLoadBalancerArnsarg
  }

-- | https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-vpcendpointservice.html#cfn-ec2-vpcendpointservice-acceptancerequired
ecvpcesAcceptanceRequired :: Lens' EC2VPCEndpointService (Maybe (Val Bool))
ecvpcesAcceptanceRequired = lens _eC2VPCEndpointServiceAcceptanceRequired (\s a -> s { _eC2VPCEndpointServiceAcceptanceRequired = a })

-- | https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-vpcendpointservice.html#cfn-ec2-vpcendpointservice-networkloadbalancerarns
ecvpcesNetworkLoadBalancerArns :: Lens' EC2VPCEndpointService (ValList Text)
ecvpcesNetworkLoadBalancerArns = lens _eC2VPCEndpointServiceNetworkLoadBalancerArns (\s a -> s { _eC2VPCEndpointServiceNetworkLoadBalancerArns = a })
