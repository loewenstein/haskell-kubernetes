-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.HostPathVolumeSource
    ( HostPathVolumeSource (..)
    , path
    , mkHostPathVolumeSource
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

-- | Represents a host path mapped into a pod. Host path volumes do not support ownership management or SELinux relabeling.
data HostPathVolumeSource = HostPathVolumeSource
    { _path :: !(Text)
    } deriving (Show, Eq, Generic)

makeLenses ''HostPathVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''HostPathVolumeSource)

instance Arbitrary HostPathVolumeSource where
    arbitrary = HostPathVolumeSource <$> arbitrary

-- | Use this method to build a HostPathVolumeSource
mkHostPathVolumeSource :: Text -> HostPathVolumeSource
mkHostPathVolumeSource xpathx = HostPathVolumeSource xpathx
