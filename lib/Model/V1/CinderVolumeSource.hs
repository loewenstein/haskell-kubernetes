{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.CinderVolumeSource
    ( CinderVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data CinderVolumeSource = CinderVolumeSource
    { _volumeID :: Text
    , _fsType :: Text
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''CinderVolumeSource

instance FromJSON CinderVolumeSource
instance ToJSON CinderVolumeSource
instance Arbitrary CinderVolumeSource where
    arbitrary = CinderVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
