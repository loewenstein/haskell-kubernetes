-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LimitRangeItem
    ( LimitRangeItem (..)
    , type_
    , max
    , min
    , default_
    , defaultRequest
    , maxLimitRequestRatio
    , mkLimitRangeItem
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Kubernetes.Model.V1.Any   (Any)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | LimitRangeItem defines a min/max usage limit for any resource that matches on kind.
data LimitRangeItem = LimitRangeItem
    { _type_                :: !(Maybe Text)
    , _max                  :: !(Maybe Any)
    , _min                  :: !(Maybe Any)
    , _default_             :: !(Maybe Any)
    , _defaultRequest       :: !(Maybe Any)
    , _maxLimitRequestRatio :: !(Maybe Any)
    } deriving (Show, Eq, Generic)

makeLenses ''LimitRangeItem

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''LimitRangeItem)

instance Arbitrary LimitRangeItem where
    arbitrary = LimitRangeItem <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a LimitRangeItem
mkLimitRangeItem :: LimitRangeItem
mkLimitRangeItem = LimitRangeItem Nothing Nothing Nothing Nothing Nothing Nothing
