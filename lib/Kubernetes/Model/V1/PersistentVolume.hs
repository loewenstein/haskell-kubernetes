-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolume
    ( PersistentVolume (..)
    , kind
    , apiVersion
    , metadata
    , spec
    , status
    ) where

import           Control.Lens.TH                            (makeLenses)
import           Data.Aeson.TH                              (defaultOptions,
                                                             deriveJSON,
                                                             fieldLabelModifier)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)
import           Kubernetes.Model.V1.ObjectMeta             (ObjectMeta)
import           Kubernetes.Model.V1.PersistentVolumeSpec   (PersistentVolumeSpec)
import           Kubernetes.Model.V1.PersistentVolumeStatus (PersistentVolumeStatus)
import           Prelude                                    hiding (drop, error,
                                                             max, min)
import qualified Prelude                                    as P
import           Test.QuickCheck                            (Arbitrary,
                                                             arbitrary)
import           Test.QuickCheck.Instances                  ()

-- | PersistentVolume (PV) is a storage resource provisioned by an administrator. It is analogous to a node. More info: http://releases.k8s.io/HEAD/docs/user-guide/persistent-volumes.md
data PersistentVolume = PersistentVolume
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _spec       :: Maybe PersistentVolumeSpec
    , _status     :: Maybe PersistentVolumeStatus
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolume

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PersistentVolume)

instance Arbitrary PersistentVolume where
    arbitrary = PersistentVolume <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
