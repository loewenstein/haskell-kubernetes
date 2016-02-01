{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerState
    ( ContainerState (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ContainerStateRunning
import Model.V1.ContainerStateTerminated
import Model.V1.ContainerStateWaiting


data ContainerState = ContainerState
    { _waiting :: ContainerStateWaiting
    , _running :: ContainerStateRunning
    , _terminated :: ContainerStateTerminated
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerState

instance FromJSON ContainerState
instance ToJSON ContainerState
instance Arbitrary ContainerState where
    arbitrary = ContainerState <$> arbitrary <*> arbitrary <*> arbitrary
