-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerStatus
    ( ContainerStatus (..)
    , name
    , state
    , lastState
    , ready
    , restartCount
    , image
    , imageID
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
import           Kubernetes.Model.V1.ContainerState (ContainerState)

-- | ContainerStatus contains details for the current status of this container.
data ContainerStatus = ContainerStatus
    { _name :: Text
    , _state :: Maybe ContainerState
    , _lastState :: Maybe ContainerState
    , _ready :: Bool
    , _restartCount :: Integer
    , _image :: Text
    , _imageID :: Text
    , _containerID :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerStatus

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerStatus)

instance Arbitrary ContainerStatus where
    arbitrary = ContainerStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
