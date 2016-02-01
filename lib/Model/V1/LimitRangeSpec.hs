{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LimitRangeSpec
    ( LimitRangeSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LimitRangeItem


data LimitRangeSpec = LimitRangeSpec
    { _limits :: [LimitRangeItem]
    } deriving (Show, Eq, Generic)
makeLenses ''LimitRangeSpec

instance FromJSON LimitRangeSpec
instance ToJSON LimitRangeSpec
instance Arbitrary LimitRangeSpec where
    arbitrary = LimitRangeSpec <$> arbitrary
