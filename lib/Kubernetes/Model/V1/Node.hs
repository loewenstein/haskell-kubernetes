-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Node
    ( Node (..)
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
import           Kubernetes.Model.V1.NodeSpec (NodeSpec)
import           Kubernetes.Model.V1.NodeStatus (NodeStatus)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)

-- | Node is a worker node in Kubernetes, formerly known as minion. Each node will have a unique identifier in the cache (i.e. in etcd).
data Node = Node
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _spec :: Maybe NodeSpec
    , _status :: Maybe NodeStatus
    } deriving (Show, Eq, Generic)

makeLenses ''Node

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Node)

instance Arbitrary Node where
    arbitrary = Node <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
