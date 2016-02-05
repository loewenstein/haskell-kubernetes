-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.SecretKeySelector
    ( SecretKeySelector (..)
    , name
    , key
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

-- | SecretKeySelector selects a key of a Secret.
data SecretKeySelector = SecretKeySelector
    { _name :: Maybe Text
    , _key  :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''SecretKeySelector

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''SecretKeySelector)

instance Arbitrary SecretKeySelector where
    arbitrary = SecretKeySelector <$> arbitrary <*> arbitrary
