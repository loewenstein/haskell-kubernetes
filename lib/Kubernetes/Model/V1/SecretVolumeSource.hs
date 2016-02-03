-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.SecretVolumeSource
    ( SecretVolumeSource (..)
    , secretName
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Adapts a Secret into a volume.\n\nThe contents of the target Secret&#39;s Data field will be presented in a volume as files using the keys in the Data field as the file names. Secret volumes support ownership management and SELinux relabeling.
data SecretVolumeSource = SecretVolumeSource
    { _secretName :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''SecretVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''SecretVolumeSource)

instance Arbitrary SecretVolumeSource where
    arbitrary = SecretVolumeSource <$> arbitrary
