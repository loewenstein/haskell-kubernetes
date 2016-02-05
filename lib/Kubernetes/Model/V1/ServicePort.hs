-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ServicePort
    ( ServicePort (..)
    , name
    , protocol
    , port
    , targetPort
    , nodePort
    , mkServicePort
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Kubernetes.Utils          (IntegerOrText)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ServicePort conatins information on service&#39;s port.
data ServicePort = ServicePort
    { _name       :: Maybe Text
    , _protocol   :: Maybe Text
    , _port       :: Integer
    , _targetPort :: Maybe IntegerOrText
    , _nodePort   :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''ServicePort

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ServicePort)

instance Arbitrary ServicePort where
    arbitrary = ServicePort <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a ServicePort
mkServicePort :: Integer -> ServicePort
mkServicePort xportx = ServicePort Nothing Nothing xportx Nothing Nothing
