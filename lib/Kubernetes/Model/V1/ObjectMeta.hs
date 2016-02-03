-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ObjectMeta
    ( ObjectMeta (..)
    , name
    , generateName
    , namespace
    , selfLink
    , uid
    , resourceVersion
    , generation
    , creationTimestamp
    , deletionTimestamp
    , deletionGracePeriodSeconds
    , labels
    , annotations
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.Any (Any)

-- | ObjectMeta is metadata that all persisted resources must have, which includes all objects users must create.
data ObjectMeta = ObjectMeta
    { _name :: Maybe Text
    , _generateName :: Maybe Text
    , _namespace :: Maybe Text
    , _selfLink :: Maybe Text
    , _uid :: Maybe Text
    , _resourceVersion :: Maybe Text
    , _generation :: Maybe Integer
    , _creationTimestamp :: Maybe Text
    , _deletionTimestamp :: Maybe Text
    , _deletionGracePeriodSeconds :: Maybe Integer
    , _labels :: Maybe Any
    , _annotations :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''ObjectMeta

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ObjectMeta)

instance Arbitrary ObjectMeta where
    arbitrary = ObjectMeta <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
