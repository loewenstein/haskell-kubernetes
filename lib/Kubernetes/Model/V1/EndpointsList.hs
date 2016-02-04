-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EndpointsList
    ( EndpointsList (..)
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
import           Kubernetes.Model.V1.Endpoints         (Endpoints)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | EndpointsList is a list of endpoints.
data EndpointsList = EndpointsList
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _items      :: [Endpoints]
    } deriving (Show, Eq, Generic)

makeLenses ''EndpointsList

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''EndpointsList)

instance Arbitrary EndpointsList where
    arbitrary = EndpointsList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
