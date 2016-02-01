{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LocalObjectReference
    ( LocalObjectReference (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data LocalObjectReference = LocalObjectReference
    { _name :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''LocalObjectReference

instance FromJSON LocalObjectReference
instance ToJSON LocalObjectReference
instance Arbitrary LocalObjectReference where
    arbitrary = LocalObjectReference <$> arbitrary
