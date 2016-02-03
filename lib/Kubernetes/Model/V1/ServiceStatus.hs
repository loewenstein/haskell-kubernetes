-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ServiceStatus
    ( ServiceStatus (..)
    , loadBalancer
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.LoadBalancerStatus (LoadBalancerStatus)

-- | ServiceStatus represents the current status of a service.
data ServiceStatus = ServiceStatus
    { _loadBalancer :: Maybe LoadBalancerStatus
    } deriving (Show, Eq, Generic)

makeLenses ''ServiceStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ServiceStatus)

instance Arbitrary ServiceStatus where
    arbitrary = ServiceStatus <$> arbitrary
