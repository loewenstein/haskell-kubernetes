-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Unversioned.StatusCause
    ( StatusCause (..)
    , reason
    , message
    , field
    , mkStatusCause
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

-- | StatusCause provides more information about an api.Status failure, including cases when multiple errors are encountered.
data StatusCause = StatusCause
    { _reason  :: Maybe Text
    , _message :: Maybe Text
    , _field   :: Maybe Text
    } deriving (Show, Eq, Generic)

makeLenses ''StatusCause

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''StatusCause)

instance Arbitrary StatusCause where
    arbitrary = StatusCause <$> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a StatusCause
mkStatusCause :: StatusCause
mkStatusCause = StatusCause Nothing Nothing Nothing
