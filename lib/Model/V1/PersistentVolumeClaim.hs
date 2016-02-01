{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeClaim
    ( PersistentVolumeClaim (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.PersistentVolumeClaimSpec
import Model.V1.PersistentVolumeClaimStatus


data PersistentVolumeClaim = PersistentVolumeClaim
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: PersistentVolumeClaimSpec
    , _status :: PersistentVolumeClaimStatus
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeClaim

instance FromJSON PersistentVolumeClaim
instance ToJSON PersistentVolumeClaim
instance Arbitrary PersistentVolumeClaim where
    arbitrary = PersistentVolumeClaim <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
