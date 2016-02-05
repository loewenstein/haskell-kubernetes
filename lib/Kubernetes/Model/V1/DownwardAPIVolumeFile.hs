-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.DownwardAPIVolumeFile
    ( DownwardAPIVolumeFile (..)
    , path
    , fieldRef
    ) where

import           Control.Lens.TH                         (makeLenses)
import           Data.Aeson.TH                           (defaultOptions,
                                                          deriveJSON,
                                                          fieldLabelModifier)
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)
import           Kubernetes.Model.V1.ObjectFieldSelector (ObjectFieldSelector)
import           Prelude                                 hiding (drop, error,
                                                          max, min)
import qualified Prelude                                 as P
import           Test.QuickCheck                         (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances               ()

-- | DownwardAPIVolumeFile represents information to create the file containing the pod field
data DownwardAPIVolumeFile = DownwardAPIVolumeFile
    { _path     :: Text
    , _fieldRef :: ObjectFieldSelector
    } deriving (Show, Eq, Generic)

makeLenses ''DownwardAPIVolumeFile

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''DownwardAPIVolumeFile)

instance Arbitrary DownwardAPIVolumeFile where
    arbitrary = DownwardAPIVolumeFile <$> arbitrary <*> arbitrary
