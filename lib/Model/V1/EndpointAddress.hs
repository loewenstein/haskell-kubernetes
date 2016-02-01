{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EndpointAddress
    ( EndpointAddress (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectReference


data EndpointAddress = EndpointAddress
    { _ip :: Text
    , _targetRef :: ObjectReference
    } deriving (Show, Eq, Generic)
makeLenses ''EndpointAddress

instance FromJSON EndpointAddress
instance ToJSON EndpointAddress
instance Arbitrary EndpointAddress where
    arbitrary = EndpointAddress <$> arbitrary <*> arbitrary
