{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Endpoints
    ( Endpoints (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.EndpointSubset
import Model.V1.ObjectMeta


data Endpoints = Endpoints
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _subsets :: [EndpointSubset]
    } deriving (Show, Eq, Generic)
makeLenses ''Endpoints

instance FromJSON Endpoints
instance ToJSON Endpoints
instance Arbitrary Endpoints where
    arbitrary = Endpoints <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
