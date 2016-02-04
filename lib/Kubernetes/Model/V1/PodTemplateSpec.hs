-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodTemplateSpec
    ( PodTemplateSpec (..)
    , metadata
    , spec
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Kubernetes.Model.V1.PodSpec (PodSpec)

-- | PodTemplateSpec describes the data a pod should have when created from a template
data PodTemplateSpec = PodTemplateSpec
    { _metadata :: Maybe ObjectMeta
    , _spec :: Maybe PodSpec
    } deriving (Show, Eq, Generic)

makeLenses ''PodTemplateSpec

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PodTemplateSpec)

instance Arbitrary PodTemplateSpec where
    arbitrary = PodTemplateSpec <$> arbitrary <*> arbitrary
