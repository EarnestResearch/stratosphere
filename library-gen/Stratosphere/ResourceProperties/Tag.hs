{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iotanalytics-datastore-tag.html

module Stratosphere.ResourceProperties.Tag where

import Stratosphere.ResourceImports


-- | Full data type definition for Tag. See 'tag' for a more convenient
-- constructor.
data Tag =
  Tag
  { _tagKey :: Val Text
  , _tagValue :: Val Text
  } deriving (Show, Eq)

instance ToJSON Tag where
  toJSON Tag{..} =
    object $
    catMaybes
    [ (Just . ("Key",) . toJSON) _tagKey
    , (Just . ("Value",) . toJSON) _tagValue
    ]

instance FromJSON Tag where
  parseJSON (Object obj) =
    Tag <$>
      (obj .: "Key") <*>
      (obj .: "Value")
  parseJSON _ = mempty

-- | Constructor for 'Tag' containing required fields as arguments.
tag
  :: Val Text -- ^ 'tagKey'
  -> Val Text -- ^ 'tagValue'
  -> Tag
tag keyarg valuearg =
  Tag
  { _tagKey = keyarg
  , _tagValue = valuearg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iotanalytics-datastore-tag.html#cfn-iotanalytics-datastore-tag-key
tagKey :: Lens' Tag (Val Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iotanalytics-datastore-tag.html#cfn-iotanalytics-datastore-tag-value
tagValue :: Lens' Tag (Val Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })
