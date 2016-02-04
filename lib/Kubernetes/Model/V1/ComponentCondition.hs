-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ComponentCondition
    ( ComponentCondition (..)
    , type_
    , status
    , message
    , error
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Information about the condition of a component.
data ComponentCondition = ComponentCondition
    { _type_ :: Text
    , _status :: Text
    , _message :: Maybe Text
    , _error :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ComponentCondition

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ComponentCondition)

instance Arbitrary ComponentCondition where
    arbitrary = ComponentCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
