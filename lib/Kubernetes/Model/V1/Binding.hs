-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
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
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Kubernetes.Model.V1.ObjectReference (ObjectReference)

-- | Binding ties one object to another. For example, a pod is bound to a node by a scheduler.
data Binding = Binding
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ObjectMeta
    , _target :: ObjectReference
    } deriving (Show, Eq, Generic)

makeLenses ''Binding

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Binding)

instance Arbitrary Binding where
    arbitrary = Binding <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
