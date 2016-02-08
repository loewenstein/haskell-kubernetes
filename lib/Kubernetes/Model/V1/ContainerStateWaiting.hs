-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerStateWaiting
    ( ContainerStateWaiting (..)
    , reason
    , message
    , mkContainerStateWaiting
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Aeson.TH             (defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (drop, error, max, min)
import qualified Prelude                   as P
import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ContainerStateWaiting is a waiting state of a container.
data ContainerStateWaiting = ContainerStateWaiting
    { _reason  :: !(Maybe Text)
    , _message :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerStateWaiting

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''ContainerStateWaiting)

instance Arbitrary ContainerStateWaiting where
    arbitrary = ContainerStateWaiting <$> arbitrary <*> arbitrary

-- | Use this method to build a ContainerStateWaiting
mkContainerStateWaiting :: ContainerStateWaiting
mkContainerStateWaiting = ContainerStateWaiting Nothing Nothing
