{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeList
    ( PersistentVolumeList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.PersistentVolume
import Model.Unversioned.ListMeta


data PersistentVolumeList = PersistentVolumeList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: Unversioned.ListMeta
    , _items :: [PersistentVolume]
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeList

instance FromJSON PersistentVolumeList
instance ToJSON PersistentVolumeList
instance Arbitrary PersistentVolumeList where
    arbitrary = PersistentVolumeList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
