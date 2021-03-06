{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-medialive-channel-audioselectorsettings.html

module Stratosphere.ResourceProperties.MediaLiveChannelAudioSelectorSettings where

import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.MediaLiveChannelAudioLanguageSelection
import Stratosphere.ResourceProperties.MediaLiveChannelAudioPidSelection

-- | Full data type definition for MediaLiveChannelAudioSelectorSettings. See
-- 'mediaLiveChannelAudioSelectorSettings' for a more convenient
-- constructor.
data MediaLiveChannelAudioSelectorSettings =
  MediaLiveChannelAudioSelectorSettings
  { _mediaLiveChannelAudioSelectorSettingsAudioLanguageSelection :: Maybe MediaLiveChannelAudioLanguageSelection
  , _mediaLiveChannelAudioSelectorSettingsAudioPidSelection :: Maybe MediaLiveChannelAudioPidSelection
  } deriving (Show, Eq)

instance ToJSON MediaLiveChannelAudioSelectorSettings where
  toJSON MediaLiveChannelAudioSelectorSettings{..} =
    object $
    catMaybes
    [ fmap (("AudioLanguageSelection",) . toJSON) _mediaLiveChannelAudioSelectorSettingsAudioLanguageSelection
    , fmap (("AudioPidSelection",) . toJSON) _mediaLiveChannelAudioSelectorSettingsAudioPidSelection
    ]

-- | Constructor for 'MediaLiveChannelAudioSelectorSettings' containing
-- required fields as arguments.
mediaLiveChannelAudioSelectorSettings
  :: MediaLiveChannelAudioSelectorSettings
mediaLiveChannelAudioSelectorSettings  =
  MediaLiveChannelAudioSelectorSettings
  { _mediaLiveChannelAudioSelectorSettingsAudioLanguageSelection = Nothing
  , _mediaLiveChannelAudioSelectorSettingsAudioPidSelection = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-medialive-channel-audioselectorsettings.html#cfn-medialive-channel-audioselectorsettings-audiolanguageselection
mlcassAudioLanguageSelection :: Lens' MediaLiveChannelAudioSelectorSettings (Maybe MediaLiveChannelAudioLanguageSelection)
mlcassAudioLanguageSelection = lens _mediaLiveChannelAudioSelectorSettingsAudioLanguageSelection (\s a -> s { _mediaLiveChannelAudioSelectorSettingsAudioLanguageSelection = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-medialive-channel-audioselectorsettings.html#cfn-medialive-channel-audioselectorsettings-audiopidselection
mlcassAudioPidSelection :: Lens' MediaLiveChannelAudioSelectorSettings (Maybe MediaLiveChannelAudioPidSelection)
mlcassAudioPidSelection = lens _mediaLiveChannelAudioSelectorSettingsAudioPidSelection (\s a -> s { _mediaLiveChannelAudioSelectorSettingsAudioPidSelection = a })
