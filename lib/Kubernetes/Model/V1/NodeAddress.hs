-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeAddress
    ( NodeAddress (..)
    , type_
    , address
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | NodeAddress contains information for the node&#39;s address.
data NodeAddress = NodeAddress
    { _type_ :: Text
    , _address :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''NodeAddress

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''NodeAddress)

instance Arbitrary NodeAddress where
    arbitrary = NodeAddress <$> arbitrary <*> arbitrary
