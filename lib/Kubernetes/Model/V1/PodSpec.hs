-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.PodSpec
    ( PodSpec (..)
    , volumes
    , containers
    , restartPolicy
    , terminationGracePeriodSeconds
    , activeDeadlineSeconds
    , dnsPolicy
    , nodeSelector
    , serviceAccountName
    , serviceAccount
    , nodeName
    , hostNetwork
    , hostPID
    , hostIPC
    , securityContext
    , imagePullSecrets
    , mkPodSpec
    ) where

import           Control.Lens.TH                          (makeLenses)
import           Data.Aeson.TH                            (defaultOptions,
                                                           deriveJSON,
                                                           fieldLabelModifier)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import           Kubernetes.Model.V1.Any                  (Any)
import           Kubernetes.Model.V1.Container            (Container)
import           Kubernetes.Model.V1.LocalObjectReference (LocalObjectReference)
import           Kubernetes.Model.V1.PodSecurityContext   (PodSecurityContext)
import           Kubernetes.Model.V1.Volume               (Volume)
import           Prelude                                  hiding (drop, error,
                                                           max, min)
import qualified Prelude                                  as P
import           Test.QuickCheck                          (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                ()

-- | PodSpec is a description of a pod.
data PodSpec = PodSpec
    { _volumes                       :: Maybe [Volume]
    , _containers                    :: [Container]
    , _restartPolicy                 :: Maybe Text
    , _terminationGracePeriodSeconds :: Maybe Integer
    , _activeDeadlineSeconds         :: Maybe Integer
    , _dnsPolicy                     :: Maybe Text
    , _nodeSelector                  :: Maybe Any
    , _serviceAccountName            :: Maybe Text
    , _serviceAccount                :: Maybe Text
    , _nodeName                      :: Maybe Text
    , _hostNetwork                   :: Maybe Bool
    , _hostPID                       :: Maybe Bool
    , _hostIPC                       :: Maybe Bool
    , _securityContext               :: Maybe PodSecurityContext
    , _imagePullSecrets              :: Maybe [LocalObjectReference]
    } deriving (Show, Eq, Generic)

makeLenses ''PodSpec

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''PodSpec)

instance Arbitrary PodSpec where
    arbitrary = PodSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a PodSpec
mkPodSpec :: [Container] -> PodSpec
mkPodSpec xcontainersx = PodSpec Nothing xcontainersx Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
