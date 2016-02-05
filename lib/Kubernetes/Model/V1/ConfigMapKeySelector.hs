-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ConfigMapKeySelector
    ( ConfigMapKeySelector (..)
    , name
    , key
    , mkConfigMapKeySelector
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Selects a key from a ConfigMap.
data ConfigMapKeySelector = ConfigMapKeySelector
    { _name :: Maybe Text
    , _key  :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''ConfigMapKeySelector

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ConfigMapKeySelector)

instance Arbitrary ConfigMapKeySelector where
    arbitrary = ConfigMapKeySelector <$> arbitrary <*> arbitrary

-- | Use this method to build a ConfigMapKeySelector
mkConfigMapKeySelector :: Text -> ConfigMapKeySelector
mkConfigMapKeySelector xkeyx = ConfigMapKeySelector Nothing xkeyx
