{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeClaimSpec
    ( PersistentVolumeClaimSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.PersistentVolumeAccessMode
import Model.V1.ResourceRequirements


data PersistentVolumeClaimSpec = PersistentVolumeClaimSpec
    { _accessModes :: [PersistentVolumeAccessMode]
    , _resources :: ResourceRequirements
    , _volumeName :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeClaimSpec

instance FromJSON PersistentVolumeClaimSpec
instance ToJSON PersistentVolumeClaimSpec
instance Arbitrary PersistentVolumeClaimSpec where
    arbitrary = PersistentVolumeClaimSpec <$> arbitrary <*> arbitrary <*> arbitrary
