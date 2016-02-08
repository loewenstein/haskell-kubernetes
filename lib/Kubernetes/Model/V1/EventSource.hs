-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EventSource
    ( EventSource (..)
    , component
    , host
    , mkEventSource
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

-- | EventSource contains information for an event.
data EventSource = EventSource
    { _component :: !(Maybe Text)
    , _host      :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

makeLenses ''EventSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''EventSource)

instance Arbitrary EventSource where
    arbitrary = EventSource <$> arbitrary <*> arbitrary

-- | Use this method to build a EventSource
mkEventSource :: EventSource
mkEventSource = EventSource Nothing Nothing
