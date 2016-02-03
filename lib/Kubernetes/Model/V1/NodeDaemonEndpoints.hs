-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeDaemonEndpoints
    ( NodeDaemonEndpoints (..)
    , kubeletEndpoint
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.DaemonEndpoint (DaemonEndpoint)

-- | NodeDaemonEndpoints lists ports opened by daemons running on the Node.
data NodeDaemonEndpoints = NodeDaemonEndpoints
    { _kubeletEndpoint :: Maybe DaemonEndpoint
    } deriving (Show, Eq, Generic)

makeLenses ''NodeDaemonEndpoints

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''NodeDaemonEndpoints)

instance Arbitrary NodeDaemonEndpoints where
    arbitrary = NodeDaemonEndpoints <$> arbitrary
