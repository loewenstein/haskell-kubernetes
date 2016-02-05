-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NamespaceList
    ( NamespaceList (..)
    , kind
    , apiVersion
    , metadata
    , items
    , mkNamespaceList
    ) where

import           Control.Lens.TH                       (makeLenses)
import           Data.Aeson.TH                         (defaultOptions,
                                                        deriveJSON,
                                                        fieldLabelModifier)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.Namespace         (Namespace)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | NamespaceList is a list of Namespaces.
data NamespaceList = NamespaceList
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _items      :: [Namespace]
    } deriving (Show, Eq, Generic)

makeLenses ''NamespaceList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''NamespaceList)

instance Arbitrary NamespaceList where
    arbitrary = NamespaceList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a NamespaceList
mkNamespaceList :: [Namespace] -> NamespaceList
mkNamespaceList xitemsx = NamespaceList Nothing Nothing Nothing xitemsx
