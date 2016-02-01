{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EmptyDirVolumeSource
    ( EmptyDirVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data EmptyDirVolumeSource = EmptyDirVolumeSource
    { _medium :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''EmptyDirVolumeSource

instance FromJSON EmptyDirVolumeSource
instance ToJSON EmptyDirVolumeSource
instance Arbitrary EmptyDirVolumeSource where
    arbitrary = EmptyDirVolumeSource <$> arbitrary
