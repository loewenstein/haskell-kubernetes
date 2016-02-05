-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeClaim
    ( PersistentVolumeClaim (..)
    , kind
    , apiVersion
    , metadata
    , spec
    , status
    , mkPersistentVolumeClaim
    ) where

import           Control.Lens.TH                                 (makeLenses)
import           Data.Aeson.TH                                   (defaultOptions,
                                                                  deriveJSON, fieldLabelModifier)
import           Data.Text                                       (Text)
import           GHC.Generics                                    (Generic)
import           Kubernetes.Model.V1.ObjectMeta                  (ObjectMeta)
import           Kubernetes.Model.V1.PersistentVolumeClaimSpec   (PersistentVolumeClaimSpec)
import           Kubernetes.Model.V1.PersistentVolumeClaimStatus (PersistentVolumeClaimStatus)
import           Prelude                                         hiding (drop,
                                                                  error, max,
                                                                  min)
import qualified Prelude                                         as P
import           Test.QuickCheck                                 (Arbitrary,
                                                                  arbitrary)
import           Test.QuickCheck.Instances                       ()

-- | PersistentVolumeClaim is a user&#39;s request for and claim to a persistent volume
data PersistentVolumeClaim = PersistentVolumeClaim
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _spec       :: Maybe PersistentVolumeClaimSpec
    , _status     :: Maybe PersistentVolumeClaimStatus
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeClaim

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PersistentVolumeClaim)

instance Arbitrary PersistentVolumeClaim where
    arbitrary = PersistentVolumeClaim <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a PersistentVolumeClaim
mkPersistentVolumeClaim :: PersistentVolumeClaim
mkPersistentVolumeClaim = PersistentVolumeClaim Nothing Nothing Nothing Nothing Nothing
