{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ResourceRequirements
    ( ResourceRequirements (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any


data ResourceRequirements = ResourceRequirements
    { _limits :: Any
    , _requests :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''ResourceRequirements

instance FromJSON ResourceRequirements
instance ToJSON ResourceRequirements
instance Arbitrary ResourceRequirements where
    arbitrary = ResourceRequirements <$> arbitrary <*> arbitrary
