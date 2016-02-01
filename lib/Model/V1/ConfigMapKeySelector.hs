{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ConfigMapKeySelector
    ( ConfigMapKeySelector (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ConfigMapKeySelector = ConfigMapKeySelector
    { _name :: Text
    , _key :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ConfigMapKeySelector

instance FromJSON ConfigMapKeySelector
instance ToJSON ConfigMapKeySelector
instance Arbitrary ConfigMapKeySelector where
    arbitrary = ConfigMapKeySelector <$> arbitrary <*> arbitrary
