-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NFSVolumeSource
    ( NFSVolumeSource (..)
    , server
    , path
    , readOnly
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Represents an NFS mount that lasts the lifetime of a pod. NFS volumes do not support ownership management or SELinux relabeling.
data NFSVolumeSource = NFSVolumeSource
    { _server :: Text
    , _path :: Text
    , _readOnly :: Maybe Bool
    } deriving (Show, Eq, Generic)

makeLenses ''NFSVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''NFSVolumeSource)

instance Arbitrary NFSVolumeSource where
    arbitrary = NFSVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
