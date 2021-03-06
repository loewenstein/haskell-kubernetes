-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource
    ( PersistentVolumeClaimVolumeSource (..)
    , claimName
    , readOnly
    , mkPersistentVolumeClaimVolumeSource
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

-- | PersistentVolumeClaimVolumeSource references the user&#39;s PVC in the same namespace. This volume finds the bound PV and mounts that volume for the pod. A PersistentVolumeClaimVolumeSource is, essentially, a wrapper around another type of volume that is owned by someone else (the system).
data PersistentVolumeClaimVolumeSource = PersistentVolumeClaimVolumeSource
    { _claimName :: !(Text)
    , _readOnly  :: !(Maybe Bool)
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeClaimVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PersistentVolumeClaimVolumeSource)

instance Arbitrary PersistentVolumeClaimVolumeSource where
    arbitrary = PersistentVolumeClaimVolumeSource <$> arbitrary <*> arbitrary

-- | Use this method to build a PersistentVolumeClaimVolumeSource
mkPersistentVolumeClaimVolumeSource :: Text -> PersistentVolumeClaimVolumeSource
mkPersistentVolumeClaimVolumeSource xclaimNamex = PersistentVolumeClaimVolumeSource xclaimNamex Nothing
