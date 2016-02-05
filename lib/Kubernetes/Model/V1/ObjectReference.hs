-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ObjectReference
    ( ObjectReference (..)
    , kind
    , namespace
    , name
    , uid
    , apiVersion
    , resourceVersion
    , fieldPath
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

-- | ObjectReference contains enough information to let you inspect or modify the referred object.
data ObjectReference = ObjectReference
    { _kind            :: Maybe Text
    , _namespace       :: Maybe Text
    , _name            :: Maybe Text
    , _uid             :: Maybe Text
    , _apiVersion      :: Maybe Text
    , _resourceVersion :: Maybe Text
    , _fieldPath       :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ObjectReference

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ObjectReference)

instance Arbitrary ObjectReference where
    arbitrary = ObjectReference <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
