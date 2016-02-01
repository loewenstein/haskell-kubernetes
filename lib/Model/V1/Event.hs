{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Event
    ( Event (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.EventSource
import Model.V1.ObjectMeta
import Model.V1.ObjectReference


data Event = Event
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _involvedObject :: ObjectReference
    , _reason :: Text
    , _message :: Text
    , _source :: EventSource
    , _firstTimestamp :: Text
    , _lastTimestamp :: Text
    , _count :: Integer
    , _type_ :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''Event

instance FromJSON Event
instance ToJSON Event
instance Arbitrary Event where
    arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
