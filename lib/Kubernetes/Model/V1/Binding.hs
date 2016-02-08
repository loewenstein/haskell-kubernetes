-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Binding
    ( Binding (..)
    , kind
    , apiVersion
    , metadata
    , target
    , mkBinding
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.ObjectMeta      (ObjectMeta)
import           Kubernetes.Model.V1.ObjectReference (ObjectReference)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | Binding ties one object to another. For example, a pod is bound to a node by a scheduler.
data Binding = Binding
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ObjectMeta)
    , _target     :: !(ObjectReference)
    } deriving (Show, Eq, Generic)

makeLenses ''Binding

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Binding)

instance Arbitrary Binding where
    arbitrary = Binding <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a Binding
mkBinding :: ObjectReference -> Binding
mkBinding xtargetx = Binding Nothing Nothing Nothing xtargetx
