-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ContainerImage
    ( ContainerImage (..)
    , repoTags
    , size
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | Describe a container image
data ContainerImage = ContainerImage
    { _repoTags :: [Text]
    , _size :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''ContainerImage

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ContainerImage)

instance Arbitrary ContainerImage where
    arbitrary = ContainerImage <$> arbitrary <*> arbitrary
