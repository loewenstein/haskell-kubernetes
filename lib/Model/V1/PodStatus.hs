{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodStatus
    ( PodStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ContainerStatus
import Model.V1.PodCondition


data PodStatus = PodStatus
    { _phase :: Text
    , _conditions :: [PodCondition]
    , _message :: Text
    , _reason :: Text
    , _hostIP :: Text
    , _podIP :: Text
    , _startTime :: Text
    , _containerStatuses :: [ContainerStatus]
    } deriving (Show, Eq, Generic)
makeLenses ''PodStatus

instance FromJSON PodStatus
instance ToJSON PodStatus
instance Arbitrary PodStatus where
    arbitrary = PodStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
