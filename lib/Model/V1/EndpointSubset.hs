{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EndpointSubset
    ( EndpointSubset (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.EndpointAddress
import Model.V1.EndpointPort


data EndpointSubset = EndpointSubset
    { _addresses :: [EndpointAddress]
    , _notReadyAddresses :: [EndpointAddress]
    , _ports :: [EndpointPort]
    } deriving (Show, Eq, Generic)
makeLenses ''EndpointSubset

instance FromJSON EndpointSubset
instance ToJSON EndpointSubset
instance Arbitrary EndpointSubset where
    arbitrary = EndpointSubset <$> arbitrary <*> arbitrary <*> arbitrary
