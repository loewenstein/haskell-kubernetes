-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeAccessMode
    ( PersistentVolumeAccessMode (..)
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)

-- | 
data PersistentVolumeAccessMode = PersistentVolumeAccessMode deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeAccessMode

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PersistentVolumeAccessMode)

instance Arbitrary PersistentVolumeAccessMode where
    arbitrary = return PersistentVolumeAccessMode
