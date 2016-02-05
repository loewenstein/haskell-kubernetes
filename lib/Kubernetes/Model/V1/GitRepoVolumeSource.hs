-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.GitRepoVolumeSource
    ( GitRepoVolumeSource (..)
    , repository
    , revision
    , directory
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

-- | Represents a volume that is populated with the contents of a git repository. Git repo volumes do not support ownership management. Git repo volumes support SELinux relabeling.
data GitRepoVolumeSource = GitRepoVolumeSource
    { _repository :: Text
    , _revision   :: Maybe Text
    , _directory  :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''GitRepoVolumeSource

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''GitRepoVolumeSource)

instance Arbitrary GitRepoVolumeSource where
    arbitrary = GitRepoVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
