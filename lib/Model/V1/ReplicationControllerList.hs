{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ReplicationControllerList
    ( ReplicationControllerList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ReplicationController
import Model.Unversioned.ListMeta


data ReplicationControllerList = ReplicationControllerList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [ReplicationController]
    } deriving (Show, Eq, Generic)
makeLenses ''ReplicationControllerList

instance FromJSON ReplicationControllerList
instance ToJSON ReplicationControllerList
instance Arbitrary ReplicationControllerList where
    arbitrary = ReplicationControllerList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
