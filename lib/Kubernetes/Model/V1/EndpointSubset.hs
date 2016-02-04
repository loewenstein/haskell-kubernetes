-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.EndpointSubset
    ( EndpointSubset (..)
    , addresses
    , notReadyAddresses
    , ports
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.EndpointAddress (EndpointAddress)
import           Kubernetes.Model.V1.EndpointPort    (EndpointPort)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | EndpointSubset is a group of addresses with a common set of ports. The expanded set of endpoints is the Cartesian product of Addresses x Ports. For example, given:\n  {\n    Addresses: [{\&quot;ip\&quot;: \&quot;10.10.1.1\&quot;}, {\&quot;ip\&quot;: \&quot;10.10.2.2\&quot;}],\n    Ports:     [{\&quot;name\&quot;: \&quot;a\&quot;, \&quot;port\&quot;: 8675}, {\&quot;name\&quot;: \&quot;b\&quot;, \&quot;port\&quot;: 309}]\n  }\nThe resulting set of endpoints can be viewed as:\n    a: [ 10.10.1.1:8675, 10.10.2.2:8675 ],\n    b: [ 10.10.1.1:309, 10.10.2.2:309 ]
data EndpointSubset = EndpointSubset
    { _addresses         :: Maybe [EndpointAddress]
    , _notReadyAddresses :: Maybe [EndpointAddress]
    , _ports             :: Maybe [EndpointPort]
    } deriving (Show, Eq, Generic)

makeLenses ''EndpointSubset

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''EndpointSubset)

instance Arbitrary EndpointSubset where
    arbitrary = EndpointSubset <$> arbitrary <*> arbitrary <*> arbitrary
