-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Namespace
    ( Namespace (..)
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
import           Kubernetes.Model.V1.NamespaceSpec (NamespaceSpec)
import           Kubernetes.Model.V1.NamespaceStatus (NamespaceStatus)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)

-- | Namespace provides a scope for Names. Use of multiple namespaces is optional.
data Namespace = Namespace
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _spec :: Maybe NamespaceSpec
    , _status :: Maybe NamespaceStatus
    } deriving (Show, Eq, Generic)

makeLenses ''Namespace

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Namespace)

instance Arbitrary Namespace where
    arbitrary = Namespace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
