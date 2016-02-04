-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ServiceList
    ( ServiceList (..)
    , kind
    , apiVersion
    , metadata
    , items
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.Service (Service)

-- | ServiceList holds a list of services.
data ServiceList = ServiceList
    { _kind :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata :: Maybe ListMeta
    , _items :: [Service]
    } deriving (Show, Eq, Generic)

makeLenses ''ServiceList

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ServiceList)

instance Arbitrary ServiceList where
    arbitrary = ServiceList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
