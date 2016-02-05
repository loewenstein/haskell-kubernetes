-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.DaemonEndpoint
    ( DaemonEndpoint (..)
    , port
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | DaemonEndpoint contains information about a single Daemon endpoint.
data DaemonEndpoint = DaemonEndpoint
    { _port :: Integer
    } deriving (Show, Eq, Generic)

makeLenses ''DaemonEndpoint

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''DaemonEndpoint)

instance Arbitrary DaemonEndpoint where
    arbitrary = DaemonEndpoint <$> arbitrary
