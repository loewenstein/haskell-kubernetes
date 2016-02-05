-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ConfigMap
    ( ConfigMap (..)
    , kind
    , apiVersion
    , metadata
    , data_
    , mkConfigMap
    ) where

import           Control.Lens.TH                (makeLenses)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           Kubernetes.Model.V1.Any        (Any)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Prelude                        hiding (drop, error, max, min)
import qualified Prelude                        as P
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances      ()

-- | ConfigMap holds configuration data for pods to consume.
data ConfigMap = ConfigMap
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _data_      :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''ConfigMap

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ConfigMap)

instance Arbitrary ConfigMap where
    arbitrary = ConfigMap <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a ConfigMap
mkConfigMap :: ConfigMap
mkConfigMap = ConfigMap Nothing Nothing Nothing Nothing
