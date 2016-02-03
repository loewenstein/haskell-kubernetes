-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LimitRangeSpec
    ( LimitRangeSpec (..)
    , limits
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.LimitRangeItem (LimitRangeItem)

-- | LimitRangeSpec defines a min/max usage limit for resources that match on kind.
data LimitRangeSpec = LimitRangeSpec
    { _limits :: [LimitRangeItem]
    } deriving (Show, Eq, Generic)

makeLenses ''LimitRangeSpec

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''LimitRangeSpec)

instance Arbitrary LimitRangeSpec where
    arbitrary = LimitRangeSpec <$> arbitrary
