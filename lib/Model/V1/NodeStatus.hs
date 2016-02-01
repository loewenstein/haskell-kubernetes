{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeStatus
    ( NodeStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.ContainerImage
import Model.V1.NodeAddress
import Model.V1.NodeCondition
import Model.V1.NodeDaemonEndpoints
import Model.V1.NodeSystemInfo


data NodeStatus = NodeStatus
    { _capacity :: Any
    , _allocatable :: Any
    , _phase :: Text
    , _conditions :: [NodeCondition]
    , _addresses :: [NodeAddress]
    , _daemonEndpoints :: NodeDaemonEndpoints
    , _nodeInfo :: NodeSystemInfo
    , _images :: [ContainerImage]
    } deriving (Show, Eq, Generic)
makeLenses ''NodeStatus

instance FromJSON NodeStatus
instance ToJSON NodeStatus
instance Arbitrary NodeStatus where
    arbitrary = NodeStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
