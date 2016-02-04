-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.FlockerVolumeSource
    ( FlockerVolumeSource (..)
    , datasetName
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Represents a Flocker volume mounted by the Flocker agent. Flocker volumes do not support ownership management or SELinux relabeling.
data FlockerVolumeSource = FlockerVolumeSource
    { _datasetName :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''FlockerVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''FlockerVolumeSource)

instance Arbitrary FlockerVolumeSource where
    arbitrary = FlockerVolumeSource <$> arbitrary
