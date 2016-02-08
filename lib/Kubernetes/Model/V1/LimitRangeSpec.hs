-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LimitRangeSpec
    ( LimitRangeSpec (..)
    , limits
    , mkLimitRangeSpec
    ) where

import           Control.Lens.TH                    (makeLenses)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           GHC.Generics                       (Generic)
import           Kubernetes.Model.V1.LimitRangeItem (LimitRangeItem)
import           Prelude                            hiding (drop, error, max,
                                                     min)
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances          ()

-- | LimitRangeSpec defines a min/max usage limit for resources that match on kind.
data LimitRangeSpec = LimitRangeSpec
    { _limits :: !([LimitRangeItem])
    } deriving (Show, Eq, Generic)

makeLenses ''LimitRangeSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''LimitRangeSpec)

instance Arbitrary LimitRangeSpec where
    arbitrary = LimitRangeSpec <$> arbitrary

-- | Use this method to build a LimitRangeSpec
mkLimitRangeSpec :: [LimitRangeItem] -> LimitRangeSpec
mkLimitRangeSpec xlimitsx = LimitRangeSpec xlimitsx
