{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ServiceAccount
    ( ServiceAccount (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LocalObjectReference
import Model.V1.ObjectMeta
import Model.V1.ObjectReference


data ServiceAccount = ServiceAccount
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _secrets :: [ObjectReference]
    , _imagePullSecrets :: [LocalObjectReference]
    } deriving (Show, Eq, Generic)
makeLenses ''ServiceAccount

instance FromJSON ServiceAccount
instance ToJSON ServiceAccount
instance Arbitrary ServiceAccount where
    arbitrary = ServiceAccount <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
