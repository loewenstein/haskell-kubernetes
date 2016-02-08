-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeDaemonEndpoints
    ( NodeDaemonEndpoints (..)
    , kubeletEndpoint
    , mkNodeDaemonEndpoints
    ) where

import           Control.Lens.TH                    (makeLenses)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           GHC.Generics                       (Generic)
import           Kubernetes.Model.V1.DaemonEndpoint (DaemonEndpoint)
import           Prelude                            hiding (drop, error, max,
                                                     min)
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances          ()

-- | NodeDaemonEndpoints lists ports opened by daemons running on the Node.
data NodeDaemonEndpoints = NodeDaemonEndpoints
    { _kubeletEndpoint :: !(Maybe DaemonEndpoint)
    } deriving (Show, Eq, Generic)

makeLenses ''NodeDaemonEndpoints

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''NodeDaemonEndpoints)

instance Arbitrary NodeDaemonEndpoints where
    arbitrary = NodeDaemonEndpoints <$> arbitrary

-- | Use this method to build a NodeDaemonEndpoints
mkNodeDaemonEndpoints :: NodeDaemonEndpoints
mkNodeDaemonEndpoints = NodeDaemonEndpoints Nothing
