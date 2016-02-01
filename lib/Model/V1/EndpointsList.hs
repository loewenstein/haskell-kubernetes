{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EndpointsList
    ( EndpointsList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Endpoints
import Model.Unversioned.ListMeta


data EndpointsList = EndpointsList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [Endpoints]
    } deriving (Show, Eq, Generic)
makeLenses ''EndpointsList

instance FromJSON EndpointsList
instance ToJSON EndpointsList
instance Arbitrary EndpointsList where
    arbitrary = EndpointsList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
