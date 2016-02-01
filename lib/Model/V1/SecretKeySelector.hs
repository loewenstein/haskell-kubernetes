{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.SecretKeySelector
    ( SecretKeySelector (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data SecretKeySelector = SecretKeySelector
    { _name :: Text
    , _key :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''SecretKeySelector

instance FromJSON SecretKeySelector
instance ToJSON SecretKeySelector
instance Arbitrary SecretKeySelector where
    arbitrary = SecretKeySelector <$> arbitrary <*> arbitrary
