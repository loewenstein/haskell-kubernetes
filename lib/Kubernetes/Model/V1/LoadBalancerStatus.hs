-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LoadBalancerStatus
    ( LoadBalancerStatus (..)
    , ingress
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.LoadBalancerIngress (LoadBalancerIngress)

-- | LoadBalancerStatus represents the status of a load-balancer.
data LoadBalancerStatus = LoadBalancerStatus
    { _ingress :: Maybe [LoadBalancerIngress]
    } deriving (Show, Eq, Generic)

makeLenses ''LoadBalancerStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''LoadBalancerStatus)

instance Arbitrary LoadBalancerStatus where
    arbitrary = LoadBalancerStatus <$> arbitrary
