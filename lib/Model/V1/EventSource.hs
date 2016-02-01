{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EventSource
    ( EventSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data EventSource = EventSource
    { _component :: Text
    , _host :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''EventSource

instance FromJSON EventSource
instance ToJSON EventSource
instance Arbitrary EventSource where
    arbitrary = EventSource <$> arbitrary <*> arbitrary
