-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodTemplateList
    ( PodTemplateList (..)
    , kind
    , apiVersion
    , metadata
    , items
    ) where

import           Control.Lens.TH                       (makeLenses)
import           Data.Aeson.TH                         (defaultOptions,
                                                        deriveJSON,
                                                        fieldLabelModifier)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.PodTemplate       (PodTemplate)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | PodTemplateList is a list of PodTemplates.
data PodTemplateList = PodTemplateList
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _items      :: [PodTemplate]
    } deriving (Show, Eq, Generic)

makeLenses ''PodTemplateList

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PodTemplateList)

instance Arbitrary PodTemplateList where
    arbitrary = PodTemplateList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
