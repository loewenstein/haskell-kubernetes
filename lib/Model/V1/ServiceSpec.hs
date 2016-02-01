{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ServiceSpec
    ( ServiceSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.ServicePort


data ServiceSpec = ServiceSpec
    { _ports :: [ServicePort]
    , _selector :: Value
    , _clusterIP :: Text
    , _type_ :: Text
    , _externalIPs :: [Text]
    , _deprecatedPublicIPs :: [Text]
    , _sessionAffinity :: Text
    , _loadBalancerIP :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ServiceSpec

instance FromJSON ServiceSpec
instance ToJSON ServiceSpec
instance Arbitrary ServiceSpec where
    arbitrary = ServiceSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
