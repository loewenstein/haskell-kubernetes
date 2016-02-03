-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EnvVar
    ( EnvVar (..)
    , name
    , value
    , valueFrom
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.EnvVarSource (EnvVarSource)

-- | EnvVar represents an environment variable present in a Container.
data EnvVar = EnvVar
    { _name :: Text
    , _value :: Maybe Text
    , _valueFrom :: Maybe EnvVarSource
    } deriving (Show, Eq, Generic)

makeLenses ''EnvVar

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''EnvVar)

instance Arbitrary EnvVar where
    arbitrary = EnvVar <$> arbitrary <*> arbitrary <*> arbitrary
