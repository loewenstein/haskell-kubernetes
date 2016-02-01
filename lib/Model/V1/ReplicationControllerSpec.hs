{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ReplicationControllerSpec
    ( ReplicationControllerSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.PodTemplateSpec


data ReplicationControllerSpec = ReplicationControllerSpec
    { _replicas :: Integer
    , _selector :: Value
    , _template :: PodTemplateSpec
    } deriving (Show, Eq, Generic)
makeLenses ''ReplicationControllerSpec

instance FromJSON ReplicationControllerSpec
instance ToJSON ReplicationControllerSpec
instance Arbitrary ReplicationControllerSpec where
    arbitrary = ReplicationControllerSpec <$> arbitrary <*> arbitrary <*> arbitrary
