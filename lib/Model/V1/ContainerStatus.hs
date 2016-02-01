{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerStatus
    ( ContainerStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ContainerState


data ContainerStatus = ContainerStatus
    { _name :: Text
    , _state :: ContainerState
    , _lastState :: ContainerState
    , _ready :: Bool
    , _restartCount :: Integer
    , _image :: Text
    , _imageID :: Text
    , _containerID :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerStatus

instance FromJSON ContainerStatus
instance ToJSON ContainerStatus
instance Arbitrary ContainerStatus where
    arbitrary = ContainerStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
