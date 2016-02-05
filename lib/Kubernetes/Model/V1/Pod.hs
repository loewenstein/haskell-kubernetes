-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Pod
    ( Pod (..)
    , kind
    , apiVersion
    , metadata
    , spec
    , status
    ) where

import           Control.Lens.TH                (makeLenses)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Kubernetes.Model.V1.PodSpec    (PodSpec)
import           Kubernetes.Model.V1.PodStatus  (PodStatus)
import           Prelude                        hiding (drop, error, max, min)
import qualified Prelude                        as P
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances      ()

-- | Pod is a collection of containers that can run on a host. This resource is created by clients and scheduled onto hosts.
data Pod = Pod
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _spec       :: Maybe PodSpec
    , _status     :: Maybe PodStatus
    } deriving (Show, Eq, Generic)

makeLenses ''Pod

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Pod)

instance Arbitrary Pod where
    arbitrary = Pod <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
