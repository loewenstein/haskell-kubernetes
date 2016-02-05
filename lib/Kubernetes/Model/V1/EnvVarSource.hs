-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EnvVarSource
    ( EnvVarSource (..)
    , fieldRef
    , configMapKeyRef
    , secretKeyRef
    ) where

import           Control.Lens.TH                          (makeLenses)
import           Data.Aeson.TH                            (defaultOptions,
                                                           deriveJSON,
                                                           fieldLabelModifier)
import           GHC.Generics                             (Generic)
import           Kubernetes.Model.V1.ConfigMapKeySelector (ConfigMapKeySelector)
import           Kubernetes.Model.V1.ObjectFieldSelector  (ObjectFieldSelector)
import           Kubernetes.Model.V1.SecretKeySelector    (SecretKeySelector)
import           Prelude                                  hiding (drop, error,
                                                           max, min)
import qualified Prelude                                  as P
import           Test.QuickCheck                          (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                ()

-- | EnvVarSource represents a source for the value of an EnvVar.
data EnvVarSource = EnvVarSource
    { _fieldRef        :: Maybe ObjectFieldSelector
    , _configMapKeyRef :: Maybe ConfigMapKeySelector
    , _secretKeyRef    :: Maybe SecretKeySelector
    } deriving (Show, Eq, Generic)

makeLenses ''EnvVarSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''EnvVarSource)

instance Arbitrary EnvVarSource where
    arbitrary = EnvVarSource <$> arbitrary <*> arbitrary <*> arbitrary
