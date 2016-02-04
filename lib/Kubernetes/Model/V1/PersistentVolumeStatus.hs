-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeStatus
    ( PersistentVolumeStatus (..)
    , phase
    , message
    , reason
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | PersistentVolumeStatus is the current status of a persistent volume.
data PersistentVolumeStatus = PersistentVolumeStatus
    { _phase :: Maybe Text
    , _message :: Maybe Text
    , _reason :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PersistentVolumeStatus)

instance Arbitrary PersistentVolumeStatus where
    arbitrary = PersistentVolumeStatus <$> arbitrary <*> arbitrary <*> arbitrary
