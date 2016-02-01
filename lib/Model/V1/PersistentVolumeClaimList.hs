{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeClaimList
    ( PersistentVolumeClaimList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.PersistentVolumeClaim
import Model.Unversioned.ListMeta


data PersistentVolumeClaimList = PersistentVolumeClaimList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [PersistentVolumeClaim]
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeClaimList

instance FromJSON PersistentVolumeClaimList
instance ToJSON PersistentVolumeClaimList
instance Arbitrary PersistentVolumeClaimList where
    arbitrary = PersistentVolumeClaimList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
