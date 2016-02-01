{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeAddress
    ( NodeAddress (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NodeAddress = NodeAddress
    { _type_ :: Text
    , _address :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''NodeAddress

instance FromJSON NodeAddress
instance ToJSON NodeAddress
instance Arbitrary NodeAddress where
    arbitrary = NodeAddress <$> arbitrary <*> arbitrary
