{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeSpec
    ( NodeSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NodeSpec = NodeSpec
    { _podCIDR :: Text
    , _externalID :: Text
    , _providerID :: Text
    , _unschedulable :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''NodeSpec

instance FromJSON NodeSpec
instance ToJSON NodeSpec
instance Arbitrary NodeSpec where
    arbitrary = NodeSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
