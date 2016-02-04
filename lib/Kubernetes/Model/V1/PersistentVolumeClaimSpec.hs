-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeClaimSpec
    ( PersistentVolumeClaimSpec (..)
    , accessModes
    , resources
    , volumeName
    ) where

import           Control.Lens.TH                                (makeLenses)
import           Data.Aeson.TH                                  (defaultOptions,
                                                                 deriveJSON, fieldLabelModifier)
import           Data.Text                                      (Text)
import           GHC.Generics                                   (Generic)
import           Kubernetes.Model.V1.PersistentVolumeAccessMode (PersistentVolumeAccessMode)
import           Kubernetes.Model.V1.ResourceRequirements       (ResourceRequirements)
import           Prelude                                        hiding (drop,
                                                                 error, max,
                                                                 min)
import qualified Prelude                                        as P
import           Test.QuickCheck                                (Arbitrary,
                                                                 arbitrary)
import           Test.QuickCheck.Instances                      ()

-- | PersistentVolumeClaimSpec describes the common attributes of storage devices and allows a Source for provider-specific attributes
data PersistentVolumeClaimSpec = PersistentVolumeClaimSpec
    { _accessModes :: Maybe [PersistentVolumeAccessMode]
    , _resources   :: Maybe ResourceRequirements
    , _volumeName  :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeClaimSpec

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PersistentVolumeClaimSpec)

instance Arbitrary PersistentVolumeClaimSpec where
    arbitrary = PersistentVolumeClaimSpec <$> arbitrary <*> arbitrary <*> arbitrary
