-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ReplicationControllerSpec
    ( ReplicationControllerSpec (..)
    , replicas
    , selector
    , template
    , mkReplicationControllerSpec
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.Any             (Any)
import           Kubernetes.Model.V1.PodTemplateSpec (PodTemplateSpec)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | ReplicationControllerSpec is the specification of a replication controller.
data ReplicationControllerSpec = ReplicationControllerSpec
    { _replicas :: !(Maybe Integer)
    , _selector :: !(Maybe Any)
    , _template :: !(Maybe PodTemplateSpec)
    } deriving (Show, Eq, Generic)

makeLenses ''ReplicationControllerSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ReplicationControllerSpec)

instance Arbitrary ReplicationControllerSpec where
    arbitrary = ReplicationControllerSpec <$> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a ReplicationControllerSpec
mkReplicationControllerSpec :: ReplicationControllerSpec
mkReplicationControllerSpec = ReplicationControllerSpec Nothing Nothing Nothing
