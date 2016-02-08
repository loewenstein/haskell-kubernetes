-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.SecretList
    ( SecretList (..)
    , kind
    , apiVersion
    , metadata
    , items
    , mkSecretList
    ) where

import           Control.Lens.TH                       (makeLenses)
import           Data.Aeson.TH                         (defaultOptions,
                                                        deriveJSON,
                                                        fieldLabelModifier)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Kubernetes.Model.Unversioned.ListMeta (ListMeta)
import           Kubernetes.Model.V1.Secret            (Secret)
import           Prelude                               hiding (drop, error, max,
                                                        min)
import qualified Prelude                               as P
import           Test.QuickCheck                       (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances             ()

-- | SecretList is a list of Secret.
data SecretList = SecretList
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ListMeta)
    , _items      :: !([Secret])
    } deriving (Show, Eq, Generic)

makeLenses ''SecretList

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''SecretList)

instance Arbitrary SecretList where
    arbitrary = SecretList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a SecretList
mkSecretList :: [Secret] -> SecretList
mkSecretList xitemsx = SecretList Nothing Nothing Nothing xitemsx
