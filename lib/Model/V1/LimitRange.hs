{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.LimitRange
    ( LimitRange (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LimitRangeSpec
import Model.V1.ObjectMeta


data LimitRange = LimitRange
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: LimitRangeSpec
    } deriving (Show, Eq, Generic)
makeLenses ''LimitRange

instance FromJSON LimitRange
instance ToJSON LimitRange
instance Arbitrary LimitRange where
    arbitrary = LimitRange <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
