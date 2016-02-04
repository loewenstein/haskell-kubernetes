-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Capabilities
    ( Capabilities (..)
    , add
    , drop
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.Capability (Capability)

-- | Adds and removes POSIX capabilities from running containers.
data Capabilities = Capabilities
    { _add :: Maybe [Capability]
    , _drop :: Maybe [Capability]
    } deriving (Show, Eq, Generic)

makeLenses ''Capabilities

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Capabilities)

instance Arbitrary Capabilities where
    arbitrary = Capabilities <$> arbitrary <*> arbitrary
