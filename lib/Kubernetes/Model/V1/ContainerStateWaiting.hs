-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerStateWaiting
    ( ContainerStateWaiting (..)
    , reason
    , message
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ContainerStateWaiting is a waiting state of a container.
data ContainerStateWaiting = ContainerStateWaiting
    { _reason :: Maybe Text
    , _message :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerStateWaiting

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerStateWaiting)

instance Arbitrary ContainerStateWaiting where
    arbitrary = ContainerStateWaiting <$> arbitrary <*> arbitrary