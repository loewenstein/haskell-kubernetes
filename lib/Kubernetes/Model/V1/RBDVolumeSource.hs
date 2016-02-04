-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.RBDVolumeSource
    ( RBDVolumeSource (..)
    , monitors
    , image
    , fsType
    , pool
    , user
    , keyring
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

-- | Represents a Rados Block Device mount that lasts the lifetime of a pod. RBD volumes support ownership management and SELinux relabeling.
data RBDVolumeSource = RBDVolumeSource
    { _monitors  :: [Text]
    , _image     :: Text
    , _fsType    :: Maybe Text
    , _pool      :: Text
    , _user      :: Text
    , _keyring   :: Text
    , _secretRef :: LocalObjectReference
    , _readOnly  :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''RBDVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''RBDVolumeSource)

instance Arbitrary RBDVolumeSource where
    arbitrary = RBDVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
