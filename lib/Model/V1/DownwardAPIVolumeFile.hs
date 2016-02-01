{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.DownwardAPIVolumeFile
    ( DownwardAPIVolumeFile (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectFieldSelector


data DownwardAPIVolumeFile = DownwardAPIVolumeFile
    { _path :: Text
    , _fieldRef :: ObjectFieldSelector
    } deriving (Show, Eq, Generic)
makeLenses ''DownwardAPIVolumeFile

instance FromJSON DownwardAPIVolumeFile
instance ToJSON DownwardAPIVolumeFile
instance Arbitrary DownwardAPIVolumeFile where
    arbitrary = DownwardAPIVolumeFile <$> arbitrary <*> arbitrary
