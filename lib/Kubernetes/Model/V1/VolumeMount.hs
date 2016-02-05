-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.VolumeMount
    ( VolumeMount (..)
    , name
    , readOnly
    , mountPath
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | VolumeMount describes a mounting of a Volume within a container.
data VolumeMount = VolumeMount
    { _name      :: Text
    , _readOnly  :: Maybe Bool
    , _mountPath :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''VolumeMount

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''VolumeMount)

instance Arbitrary VolumeMount where
    arbitrary = VolumeMount <$> arbitrary <*> arbitrary <*> arbitrary
