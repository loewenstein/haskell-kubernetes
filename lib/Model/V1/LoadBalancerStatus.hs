{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LoadBalancerStatus
    ( LoadBalancerStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LoadBalancerIngress


data LoadBalancerStatus = LoadBalancerStatus
    { _ingress :: [LoadBalancerIngress]
    } deriving (Show, Eq, Generic)
makeLenses ''LoadBalancerStatus

instance FromJSON LoadBalancerStatus
instance ToJSON LoadBalancerStatus
instance Arbitrary LoadBalancerStatus where
    arbitrary = LoadBalancerStatus <$> arbitrary
