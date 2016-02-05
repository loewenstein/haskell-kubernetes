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
    , mkNamespace
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.NamespaceSpec   (NamespaceSpec)
import           Kubernetes.Model.V1.NamespaceStatus (NamespaceStatus)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | Namespace provides a scope for Names. Use of multiple namespaces is optional.
data Namespace = Namespace
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _spec       :: Maybe NamespaceSpec
    , _status     :: Maybe NamespaceStatus
    } deriving (Show, Eq, Generic)

makeLenses ''Namespace

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Namespace)

instance Arbitrary Namespace where
    arbitrary = Namespace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a Namespace
mkNamespace :: Namespace
mkNamespace = Namespace Nothing Nothing Nothing Nothing Nothing
