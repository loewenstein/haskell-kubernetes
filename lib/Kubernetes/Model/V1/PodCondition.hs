-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodCondition
    ( PodCondition (..)
    , type_
    , status
    , lastProbeTime
    , lastTransitionTime
    , reason
    , message
    , mkPodCondition
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

-- | PodCondition contains details for the current condition of this pod.
data PodCondition = PodCondition
    { _type_              :: Text
    , _status             :: Text
    , _lastProbeTime      :: Maybe Text
    , _lastTransitionTime :: Maybe Text
    , _reason             :: Maybe Text
    , _message            :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''PodCondition

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PodCondition)

instance Arbitrary PodCondition where
    arbitrary = PodCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a PodCondition
mkPodCondition :: Text -> Text -> PodCondition
mkPodCondition xtype_x xstatusx = PodCondition xtype_x xstatusx Nothing Nothing Nothing Nothing
