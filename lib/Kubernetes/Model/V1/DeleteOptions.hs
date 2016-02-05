-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.DeleteOptions
    ( DeleteOptions (..)
    , kind
    , apiVersion
    , gracePeriodSeconds
    , mkDeleteOptions
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

-- | DeleteOptions may be provided when deleting an API object
data DeleteOptions = DeleteOptions
    { _kind               :: Maybe Text
    , _apiVersion         :: Maybe Text
    , _gracePeriodSeconds :: Integer
    } deriving (Show, Eq, Generic)

makeLenses ''DeleteOptions

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''DeleteOptions)

instance Arbitrary DeleteOptions where
    arbitrary = DeleteOptions <$> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a DeleteOptions
mkDeleteOptions :: Integer -> DeleteOptions
mkDeleteOptions xgracePeriodSecondsx = DeleteOptions Nothing Nothing xgracePeriodSecondsx
