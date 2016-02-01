{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeDaemonEndpoints
    ( NodeDaemonEndpoints (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.DaemonEndpoint


data NodeDaemonEndpoints = NodeDaemonEndpoints
    { _kubeletEndpoint :: DaemonEndpoint
    } deriving (Show, Eq, Generic)
makeLenses ''NodeDaemonEndpoints

instance FromJSON NodeDaemonEndpoints
instance ToJSON NodeDaemonEndpoints
instance Arbitrary NodeDaemonEndpoints where
    arbitrary = NodeDaemonEndpoints <$> arbitrary
