{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Json.WatchEvent
    ( WatchEvent (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data WatchEvent = WatchEvent
    { __type :: Text
    , _object :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''WatchEvent

instance FromJSON WatchEvent
instance ToJSON WatchEvent
instance Arbitrary WatchEvent where
    arbitrary = WatchEvent <$> arbitrary <*> arbitrary
