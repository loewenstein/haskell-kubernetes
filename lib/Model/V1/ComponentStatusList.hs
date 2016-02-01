{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ComponentStatusList
    ( ComponentStatusList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ComponentStatus
import Model.Unversioned.ListMeta


data ComponentStatusList = ComponentStatusList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: Unversioned.ListMeta
    , _items :: [ComponentStatus]
    } deriving (Show, Eq, Generic)
makeLenses ''ComponentStatusList

instance FromJSON ComponentStatusList
instance ToJSON ComponentStatusList
instance Arbitrary ComponentStatusList where
    arbitrary = ComponentStatusList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
