{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide

module Stratosphere.ResourceProperties.EC2LaunchTemplateCapacityReservationPreference where

import Stratosphere.ResourceImports


-- | Full data type definition for
-- EC2LaunchTemplateCapacityReservationPreference. See
-- 'ec2LaunchTemplateCapacityReservationPreference' for a more convenient
-- constructor.
data EC2LaunchTemplateCapacityReservationPreference =
  EC2LaunchTemplateCapacityReservationPreference
  { 
  } deriving (Show, Eq)

instance ToJSON EC2LaunchTemplateCapacityReservationPreference where
  toJSON _ = toJSON ([] :: [String])

instance FromJSON EC2LaunchTemplateCapacityReservationPreference where
  parseJSON (Array _) = return EC2LaunchTemplateCapacityReservationPreference
  parseJSON _ = mempty

-- | Constructor for 'EC2LaunchTemplateCapacityReservationPreference'
-- containing required fields as arguments.
ec2LaunchTemplateCapacityReservationPreference
  :: EC2LaunchTemplateCapacityReservationPreference
ec2LaunchTemplateCapacityReservationPreference  =
  EC2LaunchTemplateCapacityReservationPreference
  { 
  }


