{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EndpointPort
    ( EndpointPort (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data EndpointPort = EndpointPort
    { _name :: Text
    , _port :: Integer
    , _protocol :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''EndpointPort

instance FromJSON EndpointPort
instance ToJSON EndpointPort
instance Arbitrary EndpointPort where
    arbitrary = EndpointPort <$> arbitrary <*> arbitrary <*> arbitrary
