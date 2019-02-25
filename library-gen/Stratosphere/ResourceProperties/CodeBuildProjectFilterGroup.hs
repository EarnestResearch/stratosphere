{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-codebuild-project-filtergroup.html

module Stratosphere.ResourceProperties.CodeBuildProjectFilterGroup where

import Stratosphere.ResourceImports


-- | Full data type definition for CodeBuildProjectFilterGroup. See
-- 'codeBuildProjectFilterGroup' for a more convenient constructor.
data CodeBuildProjectFilterGroup =
  CodeBuildProjectFilterGroup
  { 
  } deriving (Show, Eq)

instance ToJSON CodeBuildProjectFilterGroup where
  toJSON _ = toJSON ([] :: [String])

instance FromJSON CodeBuildProjectFilterGroup where
  parseJSON (Array _) = return CodeBuildProjectFilterGroup
  parseJSON _ = mempty

-- | Constructor for 'CodeBuildProjectFilterGroup' containing required fields
-- as arguments.
codeBuildProjectFilterGroup
  :: CodeBuildProjectFilterGroup
codeBuildProjectFilterGroup  =
  CodeBuildProjectFilterGroup
  { 
  }


