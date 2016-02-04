-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Unversioned.ListMeta
    ( ListMeta (..)
    , selfLink
    , resourceVersion
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

-- | ListMeta describes metadata that synthetic resources must have, including lists and various status objects. A resource may have only one of {ObjectMeta, ListMeta}.
data ListMeta = ListMeta
    { _selfLink        :: Maybe Text
    , _resourceVersion :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''ListMeta

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ListMeta)

instance Arbitrary ListMeta where
    arbitrary = ListMeta <$> arbitrary <*> arbitrary
