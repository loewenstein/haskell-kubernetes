-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NamespaceSpec
    ( NamespaceSpec (..)
    , finalizers
    ) where

import           Control.Lens.TH                   (makeLenses)
import           Data.Aeson.TH                     (defaultOptions, deriveJSON,
                                                    fieldLabelModifier)
import           GHC.Generics                      (Generic)
import           Kubernetes.Model.V1.FinalizerName (FinalizerName)
import           Prelude                           hiding (drop, error, max,
                                                    min)
import qualified Prelude                           as P
import           Test.QuickCheck                   (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances         ()

-- | NamespaceSpec describes the attributes on a Namespace.
data NamespaceSpec = NamespaceSpec
    { _finalizers :: Maybe [FinalizerName]
    } deriving (Show, Eq, Generic)

makeLenses ''NamespaceSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''NamespaceSpec)

instance Arbitrary NamespaceSpec where
    arbitrary = NamespaceSpec <$> arbitrary
