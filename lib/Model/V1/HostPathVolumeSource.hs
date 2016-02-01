{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.HostPathVolumeSource
    ( HostPathVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data HostPathVolumeSource = HostPathVolumeSource
    { _path :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''HostPathVolumeSource

instance FromJSON HostPathVolumeSource
instance ToJSON HostPathVolumeSource
instance Arbitrary HostPathVolumeSource where
    arbitrary = HostPathVolumeSource <$> arbitrary
