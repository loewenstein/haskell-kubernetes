{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ObjectFieldSelector
    ( ObjectFieldSelector (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ObjectFieldSelector = ObjectFieldSelector
    { _apiVersion :: Text
    , _fieldPath :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ObjectFieldSelector

instance FromJSON ObjectFieldSelector
instance ToJSON ObjectFieldSelector
instance Arbitrary ObjectFieldSelector where
    arbitrary = ObjectFieldSelector <$> arbitrary <*> arbitrary
