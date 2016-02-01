{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ComponentCondition
    ( ComponentCondition (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ComponentCondition = ComponentCondition
    { _type_ :: Text
    , _status :: Text
    , _message :: Text
    , _error :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ComponentCondition

instance FromJSON ComponentCondition
instance ToJSON ComponentCondition
instance Arbitrary ComponentCondition where
    arbitrary = ComponentCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
