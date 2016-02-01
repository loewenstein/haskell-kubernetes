{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EventList
    ( EventList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Event
import Model.Unversioned.ListMeta


data EventList = EventList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [Event]
    } deriving (Show, Eq, Generic)
makeLenses ''EventList

instance FromJSON EventList
instance ToJSON EventList
instance Arbitrary EventList where
    arbitrary = EventList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
