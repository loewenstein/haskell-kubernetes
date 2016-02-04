-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.GlusterfsVolumeSource
    ( GlusterfsVolumeSource (..)
    , endpoints
    , path
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

-- | Represents a Glusterfs mount that lasts the lifetime of a pod. Glusterfs volumes do not support ownership management or SELinux relabeling.
data GlusterfsVolumeSource = GlusterfsVolumeSource
    { _endpoints :: Text
    , _path      :: Text
    , _readOnly  :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''GlusterfsVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''GlusterfsVolumeSource)

instance Arbitrary GlusterfsVolumeSource where
    arbitrary = GlusterfsVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
