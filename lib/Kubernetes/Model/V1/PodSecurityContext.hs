-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodSecurityContext
    ( PodSecurityContext (..)
    , seLinuxOptions
    , runAsUser
    , runAsNonRoot
    , supplementalGroups
    , fsGroup
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Kubernetes.Model.V1.SELinuxOptions (SELinuxOptions)

-- | PodSecurityContext holds pod-level security attributes and common container settings. Some fields are also present in container.securityContext.  Field values of container.securityContext take precedence over field values of PodSecurityContext.
data PodSecurityContext = PodSecurityContext
    { _seLinuxOptions :: Maybe SELinuxOptions
    , _runAsUser :: Maybe Integer
    , _runAsNonRoot :: Maybe Bool
    , _supplementalGroups :: Maybe [Integer]
    , _fsGroup :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''PodSecurityContext

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''PodSecurityContext)

instance Arbitrary PodSecurityContext where
    arbitrary = PodSecurityContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
