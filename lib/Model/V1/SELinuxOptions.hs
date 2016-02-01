{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.SELinuxOptions
    ( SELinuxOptions (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data SELinuxOptions = SELinuxOptions
    { _user :: Text
    , _role :: Text
    , _type_ :: Text
    , _level :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''SELinuxOptions

instance FromJSON SELinuxOptions
instance ToJSON SELinuxOptions
instance Arbitrary SELinuxOptions where
    arbitrary = SELinuxOptions <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
