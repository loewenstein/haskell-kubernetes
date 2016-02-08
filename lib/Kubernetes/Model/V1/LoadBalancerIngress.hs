-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LoadBalancerIngress
    ( LoadBalancerIngress (..)
    , ip
    , hostname
    , mkLoadBalancerIngress
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

-- | LoadBalancerIngress represents the status of a load-balancer ingress point: traffic intended for the service should be sent to an ingress point.
data LoadBalancerIngress = LoadBalancerIngress
    { _ip       :: !(Maybe Text)
    , _hostname :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

makeLenses ''LoadBalancerIngress

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''LoadBalancerIngress)

instance Arbitrary LoadBalancerIngress where
    arbitrary = LoadBalancerIngress <$> arbitrary <*> arbitrary

-- | Use this method to build a LoadBalancerIngress
mkLoadBalancerIngress :: LoadBalancerIngress
mkLoadBalancerIngress = LoadBalancerIngress Nothing Nothing
