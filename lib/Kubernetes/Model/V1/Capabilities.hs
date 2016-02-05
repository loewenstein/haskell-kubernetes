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
    , mkCapabilities
    ) where

import           Control.Lens.TH                (makeLenses)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier)
import           GHC.Generics                   (Generic)
import           Kubernetes.Model.V1.Capability (Capability)
import           Prelude                        hiding (drop, error, max, min)
import qualified Prelude                        as P
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances      ()

-- | Adds and removes POSIX capabilities from running containers.
data Capabilities = Capabilities
    { _add  :: Maybe [Capability]
    , _drop :: Maybe [Capability]
    } deriving (Show, Eq, Generic)

makeLenses ''Capabilities

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Capabilities)

instance Arbitrary Capabilities where
    arbitrary = Capabilities <$> arbitrary <*> arbitrary

-- | Use this method to build a Capabilities
mkCapabilities :: Capabilities
mkCapabilities = Capabilities Nothing Nothing
