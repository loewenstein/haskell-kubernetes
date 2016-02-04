-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerState
    ( ContainerState (..)
    , waiting
    , running
    , terminated
    ) where

import           Control.Lens.TH                              (makeLenses)
import           Data.Aeson.TH                                (defaultOptions,
                                                               deriveJSON, fieldLabelModifier)
import           GHC.Generics                                 (Generic)
import           Kubernetes.Model.V1.ContainerStateRunning    (ContainerStateRunning)
import           Kubernetes.Model.V1.ContainerStateTerminated (ContainerStateTerminated)
import           Kubernetes.Model.V1.ContainerStateWaiting    (ContainerStateWaiting)
import           Prelude                                      hiding (drop,
                                                               error, max, min)
import qualified Prelude                                      as P
import           Test.QuickCheck                              (Arbitrary,
                                                               arbitrary)
import           Test.QuickCheck.Instances                    ()

-- | ContainerState holds a possible state of container. Only one of its members may be specified. If none of them is specified, the default one is ContainerStateWaiting.
data ContainerState = ContainerState
    { _waiting    :: Maybe ContainerStateWaiting
    , _running    :: Maybe ContainerStateRunning
    , _terminated :: Maybe ContainerStateTerminated
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerState

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerState)

instance Arbitrary ContainerState where
    arbitrary = ContainerState <$> arbitrary <*> arbitrary <*> arbitrary
