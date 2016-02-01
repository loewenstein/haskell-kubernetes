{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.VolumeMount
    ( VolumeMount (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data VolumeMount = VolumeMount
    { _name :: Text
    , _readOnly :: Bool
    , _mountPath :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''VolumeMount

instance FromJSON VolumeMount
instance ToJSON VolumeMount
instance Arbitrary VolumeMount where
    arbitrary = VolumeMount <$> arbitrary <*> arbitrary <*> arbitrary
