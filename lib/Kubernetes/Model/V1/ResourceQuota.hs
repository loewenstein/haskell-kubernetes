-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
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
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Kubernetes.Model.V1.ResourceQuotaSpec (ResourceQuotaSpec)
import           Kubernetes.Model.V1.ResourceQuotaStatus (ResourceQuotaStatus)

-- | ResourceQuota sets aggregate quota restrictions enforced per namespace
data ResourceQuota = ResourceQuota
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _spec :: Maybe ResourceQuotaSpec
    , _status :: Maybe ResourceQuotaStatus
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceQuota

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ResourceQuota)

instance Arbitrary ResourceQuota where
    arbitrary = ResourceQuota <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
