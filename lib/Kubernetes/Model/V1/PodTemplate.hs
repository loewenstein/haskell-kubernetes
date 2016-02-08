-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodTemplate
    ( PodTemplate (..)
    , kind
    , apiVersion
    , metadata
    , template
    , mkPodTemplate
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta)
import           Kubernetes.Model.V1.PodTemplateSpec (PodTemplateSpec)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | PodTemplate describes a template for creating copies of a predefined pod.
data PodTemplate = PodTemplate
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ObjectMeta)
    , _template   :: !(Maybe PodTemplateSpec)
    } deriving (Show, Eq, Generic)

makeLenses ''PodTemplate

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PodTemplate)

instance Arbitrary PodTemplate where
    arbitrary = PodTemplate <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a PodTemplate
mkPodTemplate :: PodTemplate
mkPodTemplate = PodTemplate Nothing Nothing Nothing Nothing
