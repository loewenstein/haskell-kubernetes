-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EndpointPort
    ( EndpointPort (..)
    , name
    , port
    , protocol
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

-- | EndpointPort is a tuple that describes a single port.
data EndpointPort = EndpointPort
    { _name     :: Maybe Text
    , _port     :: Integer
    , _protocol :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''EndpointPort

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''EndpointPort)

instance Arbitrary EndpointPort where
    arbitrary = EndpointPort <$> arbitrary <*> arbitrary <*> arbitrary
