-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Endpoints
    ( Endpoints (..)
    , kind
    , apiVersion
    , metadata
    , subsets
    , mkEndpoints
    ) where

import           Control.Lens.TH                    (makeLenses)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Kubernetes.Model.V1.EndpointSubset (EndpointSubset)
import           Kubernetes.Model.V1.ObjectMeta     (ObjectMeta)
import           Prelude                            hiding (drop, error, max,
                                                     min)
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances          ()

-- | Endpoints is a collection of endpoints that implement the actual service. Example:\n  Name: \&quot;mysvc\&quot;,\n  Subsets: [\n    {\n      Addresses: [{\&quot;ip\&quot;: \&quot;10.10.1.1\&quot;}, {\&quot;ip\&quot;: \&quot;10.10.2.2\&quot;}],\n      Ports: [{\&quot;name\&quot;: \&quot;a\&quot;, \&quot;port\&quot;: 8675}, {\&quot;name\&quot;: \&quot;b\&quot;, \&quot;port\&quot;: 309}]\n    },\n    {\n      Addresses: [{\&quot;ip\&quot;: \&quot;10.10.3.3\&quot;}],\n      Ports: [{\&quot;name\&quot;: \&quot;a\&quot;, \&quot;port\&quot;: 93}, {\&quot;name\&quot;: \&quot;b\&quot;, \&quot;port\&quot;: 76}]\n    },\n ]
data Endpoints = Endpoints
    { _kind       :: !(Maybe Text)
    , _apiVersion :: !(Maybe Text)
    , _metadata   :: !(Maybe ObjectMeta)
    , _subsets    :: !([EndpointSubset])
    } deriving (Show, Eq, Generic)

makeLenses ''Endpoints

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Endpoints)

instance Arbitrary Endpoints where
    arbitrary = Endpoints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a Endpoints
mkEndpoints :: [EndpointSubset] -> Endpoints
mkEndpoints xsubsetsx = Endpoints Nothing Nothing Nothing xsubsetsx
