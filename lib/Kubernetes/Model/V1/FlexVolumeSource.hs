-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.FlexVolumeSource
    ( FlexVolumeSource (..)
    , driver
    , fsType
    , secretRef
    , readOnly
    , options
    , mkFlexVolumeSource
    ) where

import           Control.Lens.TH                          (makeLenses)
import           Data.Aeson.TH                            (defaultOptions,
                                                           deriveJSON,
                                                           fieldLabelModifier)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import           Kubernetes.Model.V1.Any                  (Any)
import           Kubernetes.Model.V1.LocalObjectReference (LocalObjectReference)
import           Prelude                                  hiding (drop, error,
                                                           max, min)
import qualified Prelude                                  as P
import           Test.QuickCheck                          (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                ()

-- | FlexVolume represents a generic volume resource that is provisioned/attached using a exec based plugin. This is an alpha feature and may change in future.
data FlexVolumeSource = FlexVolumeSource
    { _driver    :: !(Text)
    , _fsType    :: !(Maybe Text)
    , _secretRef :: !(Maybe LocalObjectReference)
    , _readOnly  :: !(Maybe Bool)
    , _options   :: !(Maybe Any)
    } deriving (Show, Eq, Generic)

makeLenses ''FlexVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''FlexVolumeSource)

instance Arbitrary FlexVolumeSource where
    arbitrary = FlexVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a FlexVolumeSource
mkFlexVolumeSource :: Text -> FlexVolumeSource
mkFlexVolumeSource xdriverx = FlexVolumeSource xdriverx Nothing Nothing Nothing Nothing
