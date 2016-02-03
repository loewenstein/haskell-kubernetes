-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NamespaceStatus
    ( NamespaceStatus (..)
    , phase
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | NamespaceStatus is information about the current status of a Namespace.
data NamespaceStatus = NamespaceStatus
    { _phase :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''NamespaceStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''NamespaceStatus)

instance Arbitrary NamespaceStatus where
    arbitrary = NamespaceStatus <$> arbitrary
