{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.DownwardAPIVolumeSource
    ( DownwardAPIVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.DownwardAPIVolumeFile


data DownwardAPIVolumeSource = DownwardAPIVolumeSource
    { _items :: [DownwardAPIVolumeFile]
    } deriving (Show, Eq, Generic)
makeLenses ''DownwardAPIVolumeSource

instance FromJSON DownwardAPIVolumeSource
instance ToJSON DownwardAPIVolumeSource
instance Arbitrary DownwardAPIVolumeSource where
    arbitrary = DownwardAPIVolumeSource <$> arbitrary
