-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeCondition
    ( NodeCondition (..)
    , type_
    , status
    , lastHeartbeatTime
    , lastTransitionTime
    , reason
    , message
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | NodeCondition contains condition infromation for a node.
data NodeCondition = NodeCondition
    { _type_ :: Text
    , _status :: Text
    , _lastHeartbeatTime :: Maybe Text
    , _lastTransitionTime :: Maybe Text
    , _reason :: Maybe Text
    , _message :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''NodeCondition

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''NodeCondition)

instance Arbitrary NodeCondition where
    arbitrary = NodeCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
