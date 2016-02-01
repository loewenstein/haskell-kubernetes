{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolume
    ( PersistentVolume (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.PersistentVolumeSpec
import Model.V1.PersistentVolumeStatus


data PersistentVolume = PersistentVolume
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: PersistentVolumeSpec
    , _status :: PersistentVolumeStatus
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolume

instance FromJSON PersistentVolume
instance ToJSON PersistentVolume
instance Arbitrary PersistentVolume where
    arbitrary = PersistentVolume <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
