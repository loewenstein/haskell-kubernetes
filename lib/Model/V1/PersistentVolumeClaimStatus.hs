{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeClaimStatus
    ( PersistentVolumeClaimStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.PersistentVolumeAccessMode


data PersistentVolumeClaimStatus = PersistentVolumeClaimStatus
    { _phase :: Text
    , _accessModes :: [PersistentVolumeAccessMode]
    , _capacity :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeClaimStatus

instance FromJSON PersistentVolumeClaimStatus
instance ToJSON PersistentVolumeClaimStatus
instance Arbitrary PersistentVolumeClaimStatus where
    arbitrary = PersistentVolumeClaimStatus <$> arbitrary <*> arbitrary <*> arbitrary
