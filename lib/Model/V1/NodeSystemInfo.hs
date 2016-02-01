{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeSystemInfo
    ( NodeSystemInfo (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NodeSystemInfo = NodeSystemInfo
    { _machineID :: Text
    , _systemUUID :: Text
    , _bootID :: Text
    , _kernelVersion :: Text
    , _osImage :: Text
    , _containerRuntimeVersion :: Text
    , _kubeletVersion :: Text
    , _kubeProxyVersion :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''NodeSystemInfo

instance FromJSON NodeSystemInfo
instance ToJSON NodeSystemInfo
instance Arbitrary NodeSystemInfo where
    arbitrary = NodeSystemInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
