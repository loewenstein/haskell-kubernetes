{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LimitRangeList
    ( LimitRangeList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LimitRange
import Model.Unversioned.ListMeta


data LimitRangeList = LimitRangeList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [LimitRange]
    } deriving (Show, Eq, Generic)
makeLenses ''LimitRangeList

instance FromJSON LimitRangeList
instance ToJSON LimitRangeList
instance Arbitrary LimitRangeList where
    arbitrary = LimitRangeList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
