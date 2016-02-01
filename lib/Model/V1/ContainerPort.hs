{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerPort
    ( ContainerPort (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ContainerPort = ContainerPort
    { _name :: Text
    , _hostPort :: Integer
    , _containerPort :: Integer
    , _protocol :: Text
    , _hostIP :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerPort

instance FromJSON ContainerPort
instance ToJSON ContainerPort
instance Arbitrary ContainerPort where
    arbitrary = ContainerPort <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
