{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ServiceList
    ( ServiceList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Service
import Model.Unversioned.ListMeta


data ServiceList = ServiceList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [Service]
    } deriving (Show, Eq, Generic)
makeLenses ''ServiceList

instance FromJSON ServiceList
instance ToJSON ServiceList
instance Arbitrary ServiceList where
    arbitrary = ServiceList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
