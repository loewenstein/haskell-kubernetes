{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ServiceStatus
    ( ServiceStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LoadBalancerStatus


data ServiceStatus = ServiceStatus
    { _loadBalancer :: LoadBalancerStatus
    } deriving (Show, Eq, Generic)
makeLenses ''ServiceStatus

instance FromJSON ServiceStatus
instance ToJSON ServiceStatus
instance Arbitrary ServiceStatus where
    arbitrary = ServiceStatus <$> arbitrary
