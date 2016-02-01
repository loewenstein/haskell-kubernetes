{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Probe
    ( Probe (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ExecAction
import Model.V1.HTTPGetAction
import Model.V1.TCPSocketAction


data Probe = Probe
    { _exec :: ExecAction
    , _httpGet :: HTTPGetAction
    , _tcpSocket :: TCPSocketAction
    , _initialDelaySeconds :: Integer
    , _timeoutSeconds :: Integer
    , _periodSeconds :: Integer
    , _successThreshold :: Integer
    , _failureThreshold :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''Probe

instance FromJSON Probe
instance ToJSON Probe
instance Arbitrary Probe where
    arbitrary = Probe <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
