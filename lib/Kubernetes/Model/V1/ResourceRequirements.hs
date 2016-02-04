-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ResourceRequirements
    ( ResourceRequirements (..)
    , limits
    , requests
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.Any (Any)

-- | ResourceRequirements describes the compute resource requirements.
data ResourceRequirements = ResourceRequirements
    { _limits :: Maybe Any
    , _requests :: Maybe Any
    } deriving (Show, Eq, Generic)

makeLenses ''ResourceRequirements

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ResourceRequirements)

instance Arbitrary ResourceRequirements where
    arbitrary = ResourceRequirements <$> arbitrary <*> arbitrary
