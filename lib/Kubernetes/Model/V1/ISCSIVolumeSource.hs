-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ISCSIVolumeSource
    ( ISCSIVolumeSource (..)
    , targetPortal
    , iqn
    , lun
    , iscsiInterface
    , fsType
    , readOnly
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

-- | Represents an ISCSI disk. ISCSI volumes can only be mounted as read/write once. ISCSI volumes support ownership management and SELinux relabeling.
data ISCSIVolumeSource = ISCSIVolumeSource
    { _targetPortal   :: Text
    , _iqn            :: Text
    , _lun            :: Integer
    , _iscsiInterface :: Maybe Text
    , _fsType         :: Text
    , _readOnly       :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''ISCSIVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ISCSIVolumeSource)

instance Arbitrary ISCSIVolumeSource where
    arbitrary = ISCSIVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
