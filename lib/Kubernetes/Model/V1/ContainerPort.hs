-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerPort
    ( ContainerPort (..)
    , name
    , hostPort
    , containerPort
    , protocol
    , hostIP
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ContainerPort represents a network port in a single container.
data ContainerPort = ContainerPort
    { _name :: Maybe Text
    , _hostPort :: Maybe Integer
    , _containerPort :: Integer
    , _protocol :: Maybe Text
    , _hostIP :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerPort

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerPort)

instance Arbitrary ContainerPort where
    arbitrary = ContainerPort <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
