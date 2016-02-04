-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Volume
    ( Volume (..)
    , name
    , hostPath
    , emptyDir
    , gcePersistentDisk
    , awsElasticBlockStore
    , gitRepo
    , secret
    , nfs
    , iscsi
    , glusterfs
    , persistentVolumeClaim
    , rbd
    , flexVolume
    , cinder
    , cephfs
    , flocker
    , downwardAPI
    , fc
    ) where

import           Control.Lens.TH                                       (makeLenses)
import           Data.Aeson.TH                                         (defaultOptions, deriveJSON, fieldLabelModifier)
import           Data.Text                                             (Text)
import           GHC.Generics                                          (Generic)
import           Kubernetes.Model.V1.AWSElasticBlockStoreVolumeSource  (AWSElasticBlockStoreVolumeSource)
import           Kubernetes.Model.V1.CephFSVolumeSource                (CephFSVolumeSource)
import           Kubernetes.Model.V1.CinderVolumeSource                (CinderVolumeSource)
import           Kubernetes.Model.V1.DownwardAPIVolumeSource           (DownwardAPIVolumeSource)
import           Kubernetes.Model.V1.EmptyDirVolumeSource              (EmptyDirVolumeSource)
import           Kubernetes.Model.V1.FCVolumeSource                    (FCVolumeSource)
import           Kubernetes.Model.V1.FlexVolumeSource                  (FlexVolumeSource)
import           Kubernetes.Model.V1.FlockerVolumeSource               (FlockerVolumeSource)
import           Kubernetes.Model.V1.GCEPersistentDiskVolumeSource     (GCEPersistentDiskVolumeSource)
import           Kubernetes.Model.V1.GitRepoVolumeSource               (GitRepoVolumeSource)
import           Kubernetes.Model.V1.GlusterfsVolumeSource             (GlusterfsVolumeSource)
import           Kubernetes.Model.V1.HostPathVolumeSource              (HostPathVolumeSource)
import           Kubernetes.Model.V1.ISCSIVolumeSource                 (ISCSIVolumeSource)
import           Kubernetes.Model.V1.NFSVolumeSource                   (NFSVolumeSource)
import           Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource (PersistentVolumeClaimVolumeSource)
import           Kubernetes.Model.V1.RBDVolumeSource                   (RBDVolumeSource)
import           Kubernetes.Model.V1.SecretVolumeSource                (SecretVolumeSource)
import           Prelude                                               hiding
                                                                        (drop,
                                                                        error,
                                                                        max,
                                                                        min)
import qualified Prelude                                               as P
import           Test.QuickCheck                                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                             ()

-- | Volume represents a named volume in a pod that may be accessed by any container in the pod.
data Volume = Volume
    { _name                  :: Text
    , _hostPath              :: Maybe HostPathVolumeSource
    , _emptyDir              :: Maybe EmptyDirVolumeSource
    , _gcePersistentDisk     :: Maybe GCEPersistentDiskVolumeSource
    , _awsElasticBlockStore  :: Maybe AWSElasticBlockStoreVolumeSource
    , _gitRepo               :: Maybe GitRepoVolumeSource
    , _secret                :: Maybe SecretVolumeSource
    , _nfs                   :: Maybe NFSVolumeSource
    , _iscsi                 :: Maybe ISCSIVolumeSource
    , _glusterfs             :: Maybe GlusterfsVolumeSource
    , _persistentVolumeClaim :: Maybe PersistentVolumeClaimVolumeSource
    , _rbd                   :: Maybe RBDVolumeSource
    , _flexVolume            :: Maybe FlexVolumeSource
    , _cinder                :: Maybe CinderVolumeSource
    , _cephfs                :: Maybe CephFSVolumeSource
    , _flocker               :: Maybe FlockerVolumeSource
    , _downwardAPI           :: Maybe DownwardAPIVolumeSource
    , _fc                    :: Maybe FCVolumeSource
    } deriving (Show, Eq, Generic)

makeLenses ''Volume

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Volume)

instance Arbitrary Volume where
    arbitrary = Volume <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
