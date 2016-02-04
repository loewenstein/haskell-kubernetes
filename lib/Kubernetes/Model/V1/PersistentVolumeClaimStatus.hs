-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeClaimStatus
    ( PersistentVolumeClaimStatus (..)
    , phase
    , accessModes
    , capacity
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.Any (Any)
import           Kubernetes.Model.V1.PersistentVolumeAccessMode (PersistentVolumeAccessMode)

-- | PersistentVolumeClaimStatus is the current status of a persistent volume claim.
data PersistentVolumeClaimStatus = PersistentVolumeClaimStatus
    { _phase :: Maybe Text
    , _accessModes :: Maybe [PersistentVolumeAccessMode]
    , _capacity :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeClaimStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PersistentVolumeClaimStatus)

instance Arbitrary PersistentVolumeClaimStatus where
    arbitrary = PersistentVolumeClaimStatus <$> arbitrary <*> arbitrary <*> arbitrary
