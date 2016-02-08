-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PersistentVolumeClaimList
    ( PersistentVolumeClaimList (..)
    , kind
    , apiVersion
    , metadata
    , items
    , mkPersistentVolumeClaimList
    ) where

import           Control.Lens.TH                           (makeLenses)
import           Data.Aeson.TH                             (defaultOptions,
                                                            deriveJSON,
                                                            fieldLabelModifier)
import           Data.Text                                 (Text)
import           GHC.Generics                              (Generic)
import           Kubernetes.Model.Unversioned.ListMeta     (ListMeta)
import           Kubernetes.Model.V1.PersistentVolumeClaim (PersistentVolumeClaim)
import           Prelude                                   hiding (drop, error,
                                                            max, min)
import qualified Prelude                                   as P
import           Test.QuickCheck                           (Arbitrary,
                                                            arbitrary)
import           Test.QuickCheck.Instances                 ()

-- | PersistentVolumeClaimList is a list of PersistentVolumeClaim items.
data PersistentVolumeClaimList = PersistentVolumeClaimList
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ListMeta)
    , _items      :: !([PersistentVolumeClaim])
    } deriving (Show, Eq, Generic)

makeLenses ''PersistentVolumeClaimList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PersistentVolumeClaimList)

instance Arbitrary PersistentVolumeClaimList where
    arbitrary = PersistentVolumeClaimList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a PersistentVolumeClaimList
mkPersistentVolumeClaimList :: [PersistentVolumeClaim] -> PersistentVolumeClaimList
mkPersistentVolumeClaimList xitemsx = PersistentVolumeClaimList Nothing Nothing Nothing xitemsx
