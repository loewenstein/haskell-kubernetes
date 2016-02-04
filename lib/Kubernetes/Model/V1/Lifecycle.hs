-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Lifecycle
    ( Lifecycle (..)
    , postStart
    , preStop
    ) where

import           Control.Lens.TH             (makeLenses)
import           Data.Aeson.TH               (defaultOptions, deriveJSON,
                                              fieldLabelModifier)
import           GHC.Generics                (Generic)
import           Kubernetes.Model.V1.Handler (Handler)
import           Prelude                     hiding (drop, error, max, min)
import qualified Prelude                     as P
import           Test.QuickCheck             (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances   ()

-- | Lifecycle describes actions that the management system should take in response to container lifecycle events. For the PostStart and PreStop lifecycle handlers, management of the container blocks until the action is complete, unless the container process fails, in which case the handler is aborted.
data Lifecycle = Lifecycle
    { _postStart :: Maybe Handler
    , _preStop   :: Maybe Handler
    } deriving (Show, Eq, Generic)

makeLenses ''Lifecycle

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Lifecycle)

instance Arbitrary Lifecycle where
    arbitrary = Lifecycle <$> arbitrary <*> arbitrary
