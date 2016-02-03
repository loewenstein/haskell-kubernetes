-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.CinderVolumeSource
    ( CinderVolumeSource (..)
    , volumeID
    , fsType
    , readOnly
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Represents a cinder volume resource in Openstack. A Cinder volume must exist before mounting to a container. The volume must also be in the same region as the kubelet. Cinder volumes support ownership management and SELinux relabeling.
data CinderVolumeSource = CinderVolumeSource
    { _volumeID :: Text
    , _fsType :: Maybe Text
    , _readOnly :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''CinderVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''CinderVolumeSource)

instance Arbitrary CinderVolumeSource where
    arbitrary = CinderVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
