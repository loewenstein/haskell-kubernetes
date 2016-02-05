-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Unversioned.Patch
    ( Patch (..)
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH   (defaultOptions, deriveJSON,
                                  fieldLabelModifier)
import           GHC.Generics    (Generic)
import           Prelude         hiding (drop, error, max, min)
import qualified Prelude         as P
import           Test.QuickCheck (Arbitrary, arbitrary)

-- | Patch is provided to give a concrete name and type to the Kubernetes PATCH request body.
data Patch = Patch deriving (Show, Eq, Generic)

makeLenses ''Patch

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''Patch)

instance Arbitrary Patch where
    arbitrary = return Patch
