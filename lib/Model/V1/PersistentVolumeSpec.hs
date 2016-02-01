{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeSpec
    ( PersistentVolumeSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.AWSElasticBlockStoreVolumeSource
import Model.V1.Any
import Model.V1.CephFSVolumeSource
import Model.V1.CinderVolumeSource
import Model.V1.FCVolumeSource
import Model.V1.FlexVolumeSource
import Model.V1.FlockerVolumeSource
import Model.V1.GCEPersistentDiskVolumeSource
import Model.V1.GlusterfsVolumeSource
import Model.V1.HostPathVolumeSource
import Model.V1.ISCSIVolumeSource
import Model.V1.NFSVolumeSource
import Model.V1.ObjectReference
import Model.V1.PersistentVolumeAccessMode
import Model.V1.RBDVolumeSource


data PersistentVolumeSpec = PersistentVolumeSpec
    { _capacity :: Value
    , _gcePersistentDisk :: GCEPersistentDiskVolumeSource
    , _awsElasticBlockStore :: AWSElasticBlockStoreVolumeSource
    , _hostPath :: HostPathVolumeSource
    , _glusterfs :: GlusterfsVolumeSource
    , _nfs :: NFSVolumeSource
    , _rbd :: RBDVolumeSource
    , _iscsi :: ISCSIVolumeSource
    , _cinder :: CinderVolumeSource
    , _cephfs :: CephFSVolumeSource
    , _fc :: FCVolumeSource
    , _flocker :: FlockerVolumeSource
    , _flexVolume :: FlexVolumeSource
    , _accessModes :: [PersistentVolumeAccessMode]
    , _claimRef :: ObjectReference
    , _persistentVolumeReclaimPolicy :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeSpec

instance FromJSON PersistentVolumeSpec
instance ToJSON PersistentVolumeSpec
instance Arbitrary PersistentVolumeSpec where
    arbitrary = PersistentVolumeSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
