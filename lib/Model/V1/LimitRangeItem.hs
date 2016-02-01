{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LimitRangeItem
    ( LimitRangeItem (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any


data LimitRangeItem = LimitRangeItem
    { _type_ :: Text
    , _max :: Any
    , _min :: Any
    , _default_ :: Any
    , _defaultRequest :: Any
    , _maxLimitRequestRatio :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''LimitRangeItem

instance FromJSON LimitRangeItem
instance ToJSON LimitRangeItem
instance Arbitrary LimitRangeItem where
    arbitrary = LimitRangeItem <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
