-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.ExecAction
    ( ExecAction (..)
    , command
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude hiding (drop, error, max, min)
import qualified Prelude as P
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

-- | ExecAction describes a \&quot;run in container\&quot; action.
data ExecAction = ExecAction
    { _command :: Maybe [Text]
    } deriving (Show, Eq, Generic)

makeLenses ''ExecAction

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''ExecAction)

instance Arbitrary ExecAction where
    arbitrary = ExecAction <$> arbitrary
