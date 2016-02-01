{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.HTTPGetAction
    ( HTTPGetAction (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data HTTPGetAction = HTTPGetAction
    { _path :: Text
    , _port :: Text
    , _host :: Text
    , _scheme :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''HTTPGetAction

instance FromJSON HTTPGetAction
instance ToJSON HTTPGetAction
instance Arbitrary HTTPGetAction where
    arbitrary = HTTPGetAction <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
