{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NFSVolumeSource
    ( NFSVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NFSVolumeSource = NFSVolumeSource
    { _server :: Text
    , _path :: Text
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''NFSVolumeSource

instance FromJSON NFSVolumeSource
instance ToJSON NFSVolumeSource
instance Arbitrary NFSVolumeSource where
    arbitrary = NFSVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
