-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.Unversioned.StatusDetails
    ( StatusDetails (..)
    , name
    , group
    , kind
    , causes
    , retryAfterSeconds
    ) where

import           Control.Lens.TH                          (makeLenses)
import           Data.Aeson.TH                            (defaultOptions,
                                                           deriveJSON,
                                                           fieldLabelModifier)
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import           Kubernetes.Model.Unversioned.StatusCause (StatusCause)
import           Prelude                                  hiding (drop, error,
                                                           max, min)
import qualified Prelude                                  as P
import           Test.QuickCheck                          (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances                ()

-- | StatusDetails is a set of additional properties that MAY be set by the server to provide additional information about a response. The Reason field of a Status object defines what attributes will be set. Clients must ignore fields that do not match the defined type of each attribute, and should assume that any attribute may be empty, invalid, or under defined.
data StatusDetails = StatusDetails
    { _name              :: Maybe Text
    , _group             :: Maybe Text
    , _kind              :: Maybe Text
    , _causes            :: Maybe [StatusCause]
    , _retryAfterSeconds :: Maybe Integer
    } deriving (Show, Eq, Generic)

makeLenses ''StatusDetails

$(deriveJSON defaultOptions{fieldLabelModifier = P.drop 1} ''StatusDetails)

instance Arbitrary StatusDetails where
    arbitrary = StatusDetails <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
