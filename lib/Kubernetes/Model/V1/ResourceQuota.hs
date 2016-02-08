-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ResourceQuota
    ( ResourceQuota (..)
    , kind
    , apiVersion
    , metadata
    , spec
    , status
    , mkResourceQuota
    ) where

import           Control.Lens.TH                         (makeLenses)
import           Data.Aeson.TH                           (defaultOptions,
                                                          deriveJSON,
                                                          fieldLabelModifier)
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)
import           Kubernetes.Model.V1.ObjectMeta          (ObjectMeta)
import           Kubernetes.Model.V1.ResourceQuotaSpec   (ResourceQuotaSpec)
import           Kubernetes.Model.V1.ResourceQuotaStatus (ResourceQuotaStatus)
import           Prelude                                 hiding (drop, error,
                                                          max, min)
import qualified Prelude                                 as P
import           Test.QuickCheck                         (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances               ()

-- | ResourceQuota sets aggregate quota restrictions enforced per namespace
data ResourceQuota = ResourceQuota
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ObjectMeta)
    , _spec       :: !(Maybe ResourceQuotaSpec)
    , _status     :: !(Maybe ResourceQuotaStatus)
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceQuota

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ResourceQuota)

instance Arbitrary ResourceQuota where
    arbitrary = ResourceQuota <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a ResourceQuota
mkResourceQuota :: ResourceQuota
mkResourceQuota = ResourceQuota Nothing Nothing Nothing Nothing Nothing
