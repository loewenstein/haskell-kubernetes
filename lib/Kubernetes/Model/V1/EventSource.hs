-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EventSource
    ( EventSource (..)
    , component
    , host
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | EventSource contains information for an event.
data EventSource = EventSource
    { _component :: Maybe Text
    , _host :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''EventSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''EventSource)

instance Arbitrary EventSource where
    arbitrary = EventSource <$> arbitrary <*> arbitrary
