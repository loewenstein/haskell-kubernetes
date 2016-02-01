{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ReplicationController
    ( ReplicationController (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.ReplicationControllerSpec
import Model.V1.ReplicationControllerStatus


data ReplicationController = ReplicationController
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: ReplicationControllerSpec
    , _status :: ReplicationControllerStatus
    } deriving (Show, Eq, Generic)
makeLenses ''ReplicationController

instance FromJSON ReplicationController
instance ToJSON ReplicationController
instance Arbitrary ReplicationController where
    arbitrary = ReplicationController <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
