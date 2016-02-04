-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.Probe
    ( Probe (..)
    , exec
    , httpGet
    , tcpSocket
    , initialDelaySeconds
    , timeoutSeconds
    , periodSeconds
    , successThreshold
    , failureThreshold
    ) where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson.TH                       (defaultOptions,
                                                      deriveJSON,
                                                      fieldLabelModifier)
import           GHC.Generics                        (Generic)
import           Kubernetes.Model.V1.ExecAction      (ExecAction)
import           Kubernetes.Model.V1.HTTPGetAction   (HTTPGetAction)
import           Kubernetes.Model.V1.TCPSocketAction (TCPSocketAction)
import           Prelude                             hiding (drop, error, max,
                                                      min)
import qualified Prelude                             as P
import           Test.QuickCheck                     (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances           ()

-- | Probe describes a health check to be performed against a container to determine whether it is alive or ready to receive traffic.
data Probe = Probe
    { _exec                :: Maybe ExecAction
    , _httpGet             :: Maybe HTTPGetAction
    , _tcpSocket           :: Maybe TCPSocketAction
    , _initialDelaySeconds :: Maybe Integer
    , _timeoutSeconds      :: Maybe Integer
    , _periodSeconds       :: Maybe Integer
    , _successThreshold    :: Maybe Integer
    , _failureThreshold    :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''Probe

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''Probe)

instance Arbitrary Probe where
    arbitrary = Probe <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
