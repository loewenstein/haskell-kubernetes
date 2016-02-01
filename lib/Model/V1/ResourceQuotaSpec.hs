{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ResourceQuotaSpec
    ( ResourceQuotaSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any


data ResourceQuotaSpec = ResourceQuotaSpec
    { _hard :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''ResourceQuotaSpec

instance FromJSON ResourceQuotaSpec
instance ToJSON ResourceQuotaSpec
instance Arbitrary ResourceQuotaSpec where
    arbitrary = ResourceQuotaSpec <$> arbitrary
