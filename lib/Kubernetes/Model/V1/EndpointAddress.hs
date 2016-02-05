-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EndpointAddress
    ( EndpointAddress (..)
    , ip
    , targetRef
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.ObjectReference (ObjectReference)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | EndpointAddress is a tuple that describes single IP address.
data EndpointAddress = EndpointAddress
    { _ip        :: Text
    , _targetRef :: Maybe ObjectReference
    } deriving (Show, Eq, Generic)

makeLenses ''EndpointAddress

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''EndpointAddress)

instance Arbitrary EndpointAddress where
    arbitrary = EndpointAddress <$> arbitrary <*> arbitrary
