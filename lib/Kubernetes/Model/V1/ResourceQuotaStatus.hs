-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ResourceQuotaStatus
    ( ResourceQuotaStatus (..)
    , hard
    , used
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           GHC.Generics              (Generic)
import           Kubernetes.Model.V1.Any   (Any)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ResourceQuotaStatus defines the enforced hard limits and observed use.
data ResourceQuotaStatus = ResourceQuotaStatus
    { _hard :: Maybe Any
    , _used :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceQuotaStatus

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ResourceQuotaStatus)

instance Arbitrary ResourceQuotaStatus where
    arbitrary = ResourceQuotaStatus <$> arbitrary <*> arbitrary
