-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ResourceQuotaList
    ( ResourceQuotaList (..)
    , kind
    , apiVersion
    , metadata
    , items
    , mkResourceQuotaList
    ) where

import           Control.Lens.TH                       (makeLenses)
import           Data.Aeson.TH                         (defaultOptions,
                                                        deriveJSON,
                                                        fieldLabelModifier)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.ResourceQuota     (ResourceQuota)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | ResourceQuotaList is a list of ResourceQuota items.
data ResourceQuotaList = ResourceQuotaList
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _items      :: [ResourceQuota]
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceQuotaList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ResourceQuotaList)

instance Arbitrary ResourceQuotaList where
    arbitrary = ResourceQuotaList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a ResourceQuotaList
mkResourceQuotaList :: [ResourceQuota] -> ResourceQuotaList
mkResourceQuotaList xitemsx = ResourceQuotaList Nothing Nothing Nothing xitemsx
