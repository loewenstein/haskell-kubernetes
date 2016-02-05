-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ResourceQuotaSpec
    ( ResourceQuotaSpec (..)
    , hard
    , mkResourceQuotaSpec
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           GHC.Generics              (Generic)
import           Kubernetes.Model.V1.Any   (Any)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ResourceQuotaSpec defines the desired hard limits to enforce for Quota.
data ResourceQuotaSpec = ResourceQuotaSpec
    { _hard :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceQuotaSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ResourceQuotaSpec)

instance Arbitrary ResourceQuotaSpec where
    arbitrary = ResourceQuotaSpec <$> arbitrary

-- | Use this method to build a ResourceQuotaSpec
mkResourceQuotaSpec :: ResourceQuotaSpec
mkResourceQuotaSpec = ResourceQuotaSpec Nothing
