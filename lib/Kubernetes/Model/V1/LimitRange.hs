-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.LimitRange
    ( LimitRange (..)
    , kind
    , apiVersion
    , metadata
    , spec
    , mkLimitRange
    ) where

import           Control.Lens.TH                    (makeLenses)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Kubernetes.Model.V1.LimitRangeSpec (LimitRangeSpec)
import           Kubernetes.Model.V1.ObjectMeta     (ObjectMeta)
import           Prelude                            hiding (drop, error, max,
                                                     min)
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances          ()

-- | LimitRange sets resource usage limits for each kind of resource in a Namespace.
data LimitRange = LimitRange
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ObjectMeta
    , _spec       :: Maybe LimitRangeSpec
    } deriving (Show, Eq, Generic)

makeLenses ''LimitRange

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''LimitRange)

instance Arbitrary LimitRange where
    arbitrary = LimitRange <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a LimitRange
mkLimitRange :: LimitRange
mkLimitRange = LimitRange Nothing Nothing Nothing Nothing
