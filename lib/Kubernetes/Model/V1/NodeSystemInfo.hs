-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Kubernetes.Model.V1.NodeSystemInfo
    ( NodeSystemInfo (..)
    , machineID
    , systemUUID
    , bootID
    , kernelVersion
    , osImage
    , containerRuntimeVersion
    , kubeletVersion
    , kubeProxyVersion
    , mkNodeSystemInfo
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

-- | NodeSystemInfo is a set of ids/uuids to uniquely identify the node.
data NodeSystemInfo = NodeSystemInfo
    { _machineID               :: Text
    , _systemUUID              :: Text
    , _bootID                  :: Text
    , _kernelVersion           :: Text
    , _osImage                 :: Text
    , _containerRuntimeVersion :: Text
    , _kubeletVersion          :: Text
    , _kubeProxyVersion        :: Text
    } deriving (Show, Eq, Generic)

makeLenses ''NodeSystemInfo

$(deriveJSON defaultOptions{fieldLabelModifier = (\n -> if n == "_type_" then "type" else P.drop 1 n)} ''NodeSystemInfo)

instance Arbitrary NodeSystemInfo where
    arbitrary = NodeSystemInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | Use this method to build a NodeSystemInfo
mkNodeSystemInfo :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> NodeSystemInfo
mkNodeSystemInfo xmachineIDx xsystemUUIDx xbootIDx xkernelVersionx xosImagex xcontainerRuntimeVersionx xkubeletVersionx xkubeProxyVersionx = NodeSystemInfo xmachineIDx xsystemUUIDx xbootIDx xkernelVersionx xosImagex xcontainerRuntimeVersionx xkubeletVersionx xkubeProxyVersionx
