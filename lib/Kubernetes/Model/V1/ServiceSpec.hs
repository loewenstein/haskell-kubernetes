-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ServiceSpec
    ( ServiceSpec (..)
    , ports
    , selector
    , clusterIP
    , type_
    , externalIPs
    , deprecatedPublicIPs
    , sessionAffinity
    , loadBalancerIP
    ) where

import           Control.Lens.TH                 (makeLenses)
import           Data.Aeson.TH                   (defaultOptions, deriveJSON,
                                                  fieldLabelModifier)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Kubernetes.Model.V1.Any         (Any)
import           Kubernetes.Model.V1.ServicePort (ServicePort)
import           Prelude                         hiding (drop, error, max, min)
import qualified Prelude                         as P
import           Test.QuickCheck                 (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances       ()

-- | ServiceSpec describes the attributes that a user creates on a service.
data ServiceSpec = ServiceSpec
    { _ports               :: [ServicePort]
    , _selector            :: Maybe Any
    , _clusterIP           :: Maybe Text
    , _type_               :: Maybe Text
    , _externalIPs         :: Maybe [Text]
    , _deprecatedPublicIPs :: Maybe [Text]
    , _sessionAffinity     :: Maybe Text
    , _loadBalancerIP      :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ServiceSpec

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ServiceSpec)

instance Arbitrary ServiceSpec where
    arbitrary = ServiceSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
