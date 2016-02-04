-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerStateTerminated
    ( ContainerStateTerminated (..)
    , exitCode
    , signal
    , reason
    , message
    , startedAt
    , finishedAt
    , containerID
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ContainerStateTerminated is a terminated state of a container.
data ContainerStateTerminated = ContainerStateTerminated
    { _exitCode :: Integer
    , _signal :: Maybe Integer
    , _reason :: Maybe Text
    , _message :: Maybe Text
    , _startedAt :: Maybe Text
    , _finishedAt :: Maybe Text
    , _containerID :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerStateTerminated

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerStateTerminated)

instance Arbitrary ContainerStateTerminated where
    arbitrary = ContainerStateTerminated <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
