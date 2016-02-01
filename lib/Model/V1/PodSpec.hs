{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodSpec
    ( PodSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.Container
import Model.V1.LocalObjectReference
import Model.V1.PodSecurityContext
import Model.V1.Volume


data PodSpec = PodSpec
    { _volumes :: [Volume]
    , _containers :: [Container]
    , _restartPolicy :: Text
    , _terminationGracePeriodSeconds :: Integer
    , _activeDeadlineSeconds :: Integer
    , _dnsPolicy :: Text
    , _nodeSelector :: Value
    , _serviceAccountName :: Text
    , _serviceAccount :: Text
    , _nodeName :: Text
    , _hostNetwork :: Bool
    , _hostPID :: Bool
    , _hostIPC :: Bool
    , _securityContext :: PodSecurityContext
    , _imagePullSecrets :: [LocalObjectReference]
    } deriving (Show, Eq, Generic)
makeLenses ''PodSpec

instance FromJSON PodSpec
instance ToJSON PodSpec
instance Arbitrary PodSpec where
    arbitrary = PodSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
