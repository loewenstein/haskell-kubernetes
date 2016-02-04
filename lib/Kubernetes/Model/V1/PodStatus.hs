-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodStatus
    ( PodStatus (..)
    , phase
    , conditions
    , message
    , reason
    , hostIP
    , podIP
    , startTime
    , containerStatuses
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.ContainerStatus (ContainerStatus)
import           Kubernetes.Model.V1.PodCondition    (PodCondition)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | PodStatus represents information about the status of a pod. Status may trail the actual state of a system.
data PodStatus = PodStatus
    { _phase             :: Maybe Text
    , _conditions        :: Maybe [PodCondition]
    , _message           :: Maybe Text
    , _reason            :: Maybe Text
    , _hostIP            :: Maybe Text
    , _podIP             :: Maybe Text
    , _startTime         :: Maybe Text
    , _containerStatuses :: Maybe [ContainerStatus]
    } deriving (Show, Eq, Generic)

makeLenses ''PodStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PodStatus)

instance Arbitrary PodStatus where
    arbitrary = PodStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
