-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Secret
    ( Secret (..)
    , kind
    , apiVersion
    , metadata
    , data_
    , type_
    , mkSecret
    ) where

import           Control.Lens.TH                (makeLenses)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
import           Kubernetes.Model.V1.Any        (Any)
import           Kubernetes.Model.V1.ObjectMeta (ObjectMeta)
import           Prelude                        hiding (drop, error, max, min)
import qualified Prelude                        as P
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances      ()

-- | Secret holds secret data of a certain type. The total bytes of the values in the Data field must be less than MaxSecretSize bytes.
data Secret = Secret
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ObjectMeta)
    , _data_      :: !(Maybe Any)
    , _type_      :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

makeLenses ''Secret

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Secret)

instance Arbitrary Secret where
    arbitrary = Secret <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a Secret
mkSecret :: Secret
mkSecret = Secret Nothing Nothing Nothing Nothing Nothing
