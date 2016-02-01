{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Volume
    ( Volume (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.AWSElasticBlockStoreVolumeSource
import Model.V1.CephFSVolumeSource
import Model.V1.CinderVolumeSource
import Model.V1.DownwardAPIVolumeSource
import Model.V1.EmptyDirVolumeSource
import Model.V1.FCVolumeSource
import Model.V1.FlexVolumeSource
import Model.V1.FlockerVolumeSource
import Model.V1.GCEPersistentDiskVolumeSource
import Model.V1.GitRepoVolumeSource
import Model.V1.GlusterfsVolumeSource
import Model.V1.HostPathVolumeSource
import Model.V1.ISCSIVolumeSource
import Model.V1.NFSVolumeSource
import Model.V1.PersistentVolumeClaimVolumeSource
import Model.V1.RBDVolumeSource
import Model.V1.SecretVolumeSource


data Volume = Volume
    { _name :: Text
    , _hostPath :: HostPathVolumeSource
    , _emptyDir :: EmptyDirVolumeSource
    , _gcePersistentDisk :: GCEPersistentDiskVolumeSource
    , _awsElasticBlockStore :: AWSElasticBlockStoreVolumeSource
    , _gitRepo :: GitRepoVolumeSource
    , _secret :: SecretVolumeSource
    , _nfs :: NFSVolumeSource
    , _iscsi :: ISCSIVolumeSource
    , _glusterfs :: GlusterfsVolumeSource
    , _persistentVolumeClaim :: PersistentVolumeClaimVolumeSource
    , _rbd :: RBDVolumeSource
    , _flexVolume :: FlexVolumeSource
    , _cinder :: CinderVolumeSource
    , _cephfs :: CephFSVolumeSource
    , _flocker :: FlockerVolumeSource
    , _downwardAPI :: DownwardAPIVolumeSource
    , _fc :: FCVolumeSource
    } deriving (Show, Eq, Generic)
makeLenses ''Volume

instance FromJSON Volume
instance ToJSON Volume
instance Arbitrary Volume where
    arbitrary = Volume <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
