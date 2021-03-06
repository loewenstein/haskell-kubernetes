-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LimitRangeList
    ( LimitRangeList (..)
    , kind
    , apiVersion
    , metadata
    , items
    , mkLimitRangeList
    ) where

import           Control.Lens.TH                       (makeLenses)
import           Data.Aeson.TH                         (defaultOptions,
                                                        deriveJSON,
                                                        fieldLabelModifier)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.LimitRange        (LimitRange)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | LimitRangeList is a list of LimitRange items.
data LimitRangeList = LimitRangeList
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ListMeta)
    , _items      :: !([LimitRange])
    } deriving (Show, Eq, Generic)

makeLenses ''LimitRangeList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''LimitRangeList)

instance Arbitrary LimitRangeList where
    arbitrary = LimitRangeList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a LimitRangeList
mkLimitRangeList :: [LimitRange] -> LimitRangeList
mkLimitRangeList xitemsx = LimitRangeList Nothing Nothing Nothing xitemsx
