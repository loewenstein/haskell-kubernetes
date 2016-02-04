-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.HTTPGetAction
    ( HTTPGetAction (..)
    , path
    , port
    , host
    , scheme
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

-- | HTTPGetAction describes an action based on HTTP Get requests.
data HTTPGetAction = HTTPGetAction
    { _path   :: Maybe Text
    , _port   :: Text
    , _host   :: Maybe Text
    , _scheme :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''HTTPGetAction

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''HTTPGetAction)

instance Arbitrary HTTPGetAction where
    arbitrary = HTTPGetAction <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
