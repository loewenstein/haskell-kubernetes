-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ReplicationControllerList
    ( ReplicationControllerList (..)
    , kind
    , apiVersion
    , metadata
    , items
    ) where

import           Control.Lens.TH                           (makeLenses)
import           Data.Aeson.TH                             (defaultOptions,
                                                            deriveJSON,
                                                            fieldLabelModifier)
import           Data.Text                                 (Text)
import           GHC.Generics                              (Generic)
import           Kubernetes.Model.Unversioned.ListMeta     (ListMeta)
import           Kubernetes.Model.V1.ReplicationController (ReplicationController)
import           Prelude                                   hiding (drop, error,
                                                            max, min)
import qualified Prelude                                   as P
import           Test.QuickCheck                           (Arbitrary,
                                                            arbitrary)
import           Test.QuickCheck.Instances                 ()

-- | ReplicationControllerList is a collection of replication controllers.
data ReplicationControllerList = ReplicationControllerList
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _items      :: [ReplicationController]
    } deriving (Show, Eq, Generic)

makeLenses ''ReplicationControllerList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ReplicationControllerList)

instance Arbitrary ReplicationControllerList where
    arbitrary = ReplicationControllerList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
