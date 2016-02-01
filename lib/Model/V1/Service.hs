{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Service
    ( Service (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.ServiceSpec
import Model.V1.ServiceStatus


data Service = Service
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: ServiceSpec
    , _status :: ServiceStatus
    } deriving (Show, Eq, Generic)
makeLenses ''Service

instance FromJSON Service
instance ToJSON Service
instance Arbitrary Service where
    arbitrary = Service <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
