-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Unversioned.Status
    ( Status (..)
    , kind
    , apiVersion
    , metadata
    , status
    , message
    , reason
    , details
    , code
    , mkStatus
    ) where

import           Control.Lens.TH                            (makeLenses)
import           Data.Aeson.TH                              (defaultOptions,
                                                             deriveJSON,
                                                             fieldLabelModifier)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)
import           Kubernetes.Model.Unversioned.ListMeta      (ListMeta)
import           Kubernetes.Model.Unversioned.StatusDetails (StatusDetails)
import           Prelude                                    hiding (drop, error,
                                                             max, min)
import qualified Prelude                                    as P
import           Test.QuickCheck                            (Arbitrary,
                                                             arbitrary)
import           Test.QuickCheck.Instances                  ()

-- | Status is a return value for calls that don&#39;t return other objects.
data Status = Status
    { _kind       :: Maybe Text
    , _apiVersion :: Maybe Text
    , _metadata   :: Maybe ListMeta
    , _status     :: Maybe Text
    , _message    :: Maybe Text
    , _reason     :: Maybe Text
    , _details    :: Maybe StatusDetails
    , _code       :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''Status

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Status)

instance Arbitrary Status where
    arbitrary = Status <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a Status
mkStatus :: Status
mkStatus = Status Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
