-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeSpec
    ( PersistentVolumeSpec (..)
    , capacity
    , gcePersistentDisk
    , awsElasticBlockStore
    , hostPath
    , glusterfs
    , nfs
    , rbd
    , iscsi
    , cinder
    , cephfs
    , fc
    , flocker
    , flexVolume
    , accessModes
    , claimRef
    , persistentVolumeReclaimPolicy
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
import           Kubernetes.Model.V1.AWSElasticBlockStoreVolumeSource (AWSElasticBlockStoreVolumeSource)
import           Kubernetes.Model.V1.CephFSVolumeSource (CephFSVolumeSource)
import           Kubernetes.Model.V1.CinderVolumeSource (CinderVolumeSource)
import           Kubernetes.Model.V1.FCVolumeSource (FCVolumeSource)
import           Kubernetes.Model.V1.FlexVolumeSource (FlexVolumeSource)
import           Kubernetes.Model.V1.FlockerVolumeSource (FlockerVolumeSource)
import           Kubernetes.Model.V1.GCEPersistentDiskVolumeSource (GCEPersistentDiskVolumeSource)
import           Kubernetes.Model.V1.GlusterfsVolumeSource (GlusterfsVolumeSource)
import           Kubernetes.Model.V1.HostPathVolumeSource (HostPathVolumeSource)
import           Kubernetes.Model.V1.ISCSIVolumeSource (ISCSIVolumeSource)
import           Kubernetes.Model.V1.NFSVolumeSource (NFSVolumeSource)
import           Kubernetes.Model.V1.ObjectReference (ObjectReference)
import           Kubernetes.Model.V1.PersistentVolumeAccessMode (PersistentVolumeAccessMode)
import           Kubernetes.Model.V1.RBDVolumeSource (RBDVolumeSource)

-- | PersistentVolumeSpec is the specification of a persistent volume.
data PersistentVolumeSpec = PersistentVolumeSpec
    { _capacity :: Maybe Any
    , _gcePersistentDisk :: Maybe GCEPersistentDiskVolumeSource
    , _awsElasticBlockStore :: Maybe AWSElasticBlockStoreVolumeSource
    , _hostPath :: Maybe HostPathVolumeSource
    , _glusterfs :: Maybe GlusterfsVolumeSource
    , _nfs :: Maybe NFSVolumeSource
    , _rbd :: Maybe RBDVolumeSource
    , _iscsi :: Maybe ISCSIVolumeSource
    , _cinder :: Maybe CinderVolumeSource
    , _cephfs :: Maybe CephFSVolumeSource
    , _fc :: Maybe FCVolumeSource
    , _flocker :: Maybe FlockerVolumeSource
    , _flexVolume :: Maybe FlexVolumeSource
    , _accessModes :: Maybe [PersistentVolumeAccessMode]
    , _claimRef :: Maybe ObjectReference
    , _persistentVolumeReclaimPolicy :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeSpec

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PersistentVolumeSpec)

instance Arbitrary PersistentVolumeSpec where
    arbitrary = PersistentVolumeSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
