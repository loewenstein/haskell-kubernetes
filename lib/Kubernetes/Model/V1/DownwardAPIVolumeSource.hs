-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.DownwardAPIVolumeSource
    ( DownwardAPIVolumeSource (..)
    , items
    ) where

import           Control.Lens.TH                           (makeLenses)
import           Data.Aeson.TH                             (defaultOptions,
                                                            deriveJSON,
                                                            fieldLabelModifier)
import           GHC.Generics                              (Generic)
import           Kubernetes.Model.V1.DownwardAPIVolumeFile (DownwardAPIVolumeFile)
import           Prelude                                   hiding (drop, error,
                                                            max, min)
import qualified Prelude                                   as P
import           Test.QuickCheck                           (Arbitrary,
                                                            arbitrary)
import           Test.QuickCheck.Instances                 ()

-- | DownwardAPIVolumeSource represents a volume containing downward API info. Downward API volumes support ownership management and SELinux relabeling.
data DownwardAPIVolumeSource = DownwardAPIVolumeSource
    { _items :: Maybe [DownwardAPIVolumeFile]
    } deriving (Show, Eq, Generic)

makeLenses ''DownwardAPIVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''DownwardAPIVolumeSource)

instance Arbitrary DownwardAPIVolumeSource where
    arbitrary = DownwardAPIVolumeSource <$> arbitrary
