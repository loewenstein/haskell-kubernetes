{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.SecretVolumeSource
    ( SecretVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data SecretVolumeSource = SecretVolumeSource
    { _secretName :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''SecretVolumeSource

instance FromJSON SecretVolumeSource
instance ToJSON SecretVolumeSource
instance Arbitrary SecretVolumeSource where
    arbitrary = SecretVolumeSource <$> arbitrary
