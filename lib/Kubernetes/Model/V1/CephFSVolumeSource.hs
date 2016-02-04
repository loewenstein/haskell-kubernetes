-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.CephFSVolumeSource
    ( CephFSVolumeSource (..)
    , monitors
    , user
    , secretFile
    , secretRef
    , readOnly
    ) where

import           Control.Lens.TH                          (makeLenses)
import           Data.Aeson.TH                            (defaultOptions,
                                                           deriveJSON,
                                                           fieldLabelModifier)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import           Kubernetes.Model.V1.LocalObjectReference (LocalObjectReference)
import           Prelude                                  hiding (drop, error,
                                                           max, min)
import qualified Prelude                                  as P
import           Test.QuickCheck                          (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                ()

-- | Represents a Ceph Filesystem mount that lasts the lifetime of a pod Cephfs volumes do not support ownership management or SELinux relabeling.
data CephFSVolumeSource = CephFSVolumeSource
    { _monitors   :: [Text]
    , _user       :: Maybe Text
    , _secretFile :: Maybe Text
    , _secretRef  :: Maybe LocalObjectReference
    , _readOnly   :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''CephFSVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''CephFSVolumeSource)

instance Arbitrary CephFSVolumeSource where
    arbitrary = CephFSVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
