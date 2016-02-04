-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.AWSElasticBlockStoreVolumeSource
    ( AWSElasticBlockStoreVolumeSource (..)
    , volumeID
    , fsType
    , partition
    , readOnly
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Represents a Persistent Disk resource in AWS.\n\nAn AWS EBS disk must exist and be formatted before mounting to a container. The disk must also be in the same AWS zone as the kubelet. An AWS EBS disk can only be mounted as read/write once. AWS EBS volumes support ownership management and SELinux relabeling.
data AWSElasticBlockStoreVolumeSource = AWSElasticBlockStoreVolumeSource
    { _volumeID  :: Text
    , _fsType    :: Text
    , _partition :: Maybe Integer
    , _readOnly  :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''AWSElasticBlockStoreVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''AWSElasticBlockStoreVolumeSource)

instance Arbitrary AWSElasticBlockStoreVolumeSource where
    arbitrary = AWSElasticBlockStoreVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
