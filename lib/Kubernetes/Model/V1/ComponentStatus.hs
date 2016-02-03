-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ComponentStatus
    ( ComponentStatus (..)
    , kind
    , apiVersion
    , metadata
    , conditions
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.ComponentCondition (ComponentCondition)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)

-- | ComponentStatus (and ComponentStatusList) holds the cluster validation info.
data ComponentStatus = ComponentStatus
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _conditions :: Maybe [ComponentCondition]
    } deriving (Show, Eq, Generic)

makeLenses ''ComponentStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ComponentStatus)

instance Arbitrary ComponentStatus where
    arbitrary = ComponentStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
