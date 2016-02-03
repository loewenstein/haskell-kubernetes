-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Json.WatchEvent
    ( WatchEvent (..)
    , type_
    , object
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | 
data WatchEvent = WatchEvent
    { _type_ :: Maybe Text
    , _object :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''WatchEvent

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''WatchEvent)

instance Arbitrary WatchEvent where
    arbitrary = WatchEvent <$> arbitrary <*> arbitrary
