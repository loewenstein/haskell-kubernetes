-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeSpec
    ( NodeSpec (..)
    , podCIDR
    , externalID
    , providerID
    , unschedulable
    , mkNodeSpec
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

-- | NodeSpec describes the attributes that a node is created with.
data NodeSpec = NodeSpec
    { _podCIDR       :: Maybe Text
    , _externalID    :: Maybe Text
    , _providerID    :: Maybe Text
    , _unschedulable :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''NodeSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''NodeSpec)

instance Arbitrary NodeSpec where
    arbitrary = NodeSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a NodeSpec
mkNodeSpec :: NodeSpec
mkNodeSpec = NodeSpec Nothing Nothing Nothing Nothing
