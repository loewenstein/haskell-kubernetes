{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodCondition
    ( PodCondition (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data PodCondition = PodCondition
    { _type_ :: Text
    , _status :: Text
    , _lastProbeTime :: Text
    , _lastTransitionTime :: Text
    , _reason :: Text
    , _message :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''PodCondition

instance FromJSON PodCondition
instance ToJSON PodCondition
instance Arbitrary PodCondition where
    arbitrary = PodCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
