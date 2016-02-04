-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.SELinuxOptions
    ( SELinuxOptions (..)
    , user
    , role
    , type_
    , level
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

-- | SELinuxOptions are the labels to be applied to the container
data SELinuxOptions = SELinuxOptions
    { _user  :: Maybe Text
    , _role  :: Maybe Text
    , _type_ :: Maybe Text
    , _level :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''SELinuxOptions

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''SELinuxOptions)

instance Arbitrary SELinuxOptions where
    arbitrary = SELinuxOptions <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
