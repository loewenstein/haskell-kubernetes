{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NodeCondition
    ( NodeCondition (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NodeCondition = NodeCondition
    { _type_ :: Text
    , _status :: Text
    , _lastHeartbeatTime :: Text
    , _lastTransitionTime :: Text
    , _reason :: Text
    , _message :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''NodeCondition

instance FromJSON NodeCondition
instance ToJSON NodeCondition
instance Arbitrary NodeCondition where
    arbitrary = NodeCondition <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
