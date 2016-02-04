-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ServiceAccount
    ( ServiceAccount (..)
    , kind
    , apiVersion
    , metadata
    , secrets
    , imagePullSecrets
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.LocalObjectReference (LocalObjectReference)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Kubernetes.Model.V1.ObjectReference (ObjectReference)

-- | ServiceAccount binds together: * a name, understood by users, and perhaps by peripheral systems, for an identity * a principal that can be authenticated and authorized * a set of secrets
data ServiceAccount = ServiceAccount
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _secrets :: Maybe [ObjectReference]
    , _imagePullSecrets :: Maybe [LocalObjectReference]
    } deriving (Show, Eq, Generic)

makeLenses ''ServiceAccount

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ServiceAccount)

instance Arbitrary ServiceAccount where
    arbitrary = ServiceAccount <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
