-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ObjectFieldSelector
    ( ObjectFieldSelector (..)
    , apiVersion
    , fieldPath
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ObjectFieldSelector selects an APIVersioned field of an object.
data ObjectFieldSelector = ObjectFieldSelector
    { _apiVersion :: Maybe Text
    , _fieldPath :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''ObjectFieldSelector

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ObjectFieldSelector)

instance Arbitrary ObjectFieldSelector where
    arbitrary = ObjectFieldSelector <$> arbitrary <*> arbitrary
