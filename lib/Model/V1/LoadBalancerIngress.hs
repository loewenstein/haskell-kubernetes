{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LoadBalancerIngress
    ( LoadBalancerIngress (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data LoadBalancerIngress = LoadBalancerIngress
    { _ip :: Text
    , _hostname :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''LoadBalancerIngress

instance FromJSON LoadBalancerIngress
instance ToJSON LoadBalancerIngress
instance Arbitrary LoadBalancerIngress where
    arbitrary = LoadBalancerIngress <$> arbitrary <*> arbitrary
