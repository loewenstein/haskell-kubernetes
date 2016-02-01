{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ReplicationControllerStatus
    ( ReplicationControllerStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ReplicationControllerStatus = ReplicationControllerStatus
    { _replicas :: Integer
    , _observedGeneration :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''ReplicationControllerStatus

instance FromJSON ReplicationControllerStatus
instance ToJSON ReplicationControllerStatus
instance Arbitrary ReplicationControllerStatus where
    arbitrary = ReplicationControllerStatus <$> arbitrary <*> arbitrary
