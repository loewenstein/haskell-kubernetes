{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Container
    ( Container (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ContainerPort
import Model.V1.EnvVar
import Model.V1.Lifecycle
import Model.V1.Probe
import Model.V1.ResourceRequirements
import Model.V1.SecurityContext
import Model.V1.VolumeMount


data Container = Container
    { _name :: Text
    , _image :: Text
    , _command :: [Text]
    , _args :: [Text]
    , _workingDir :: Text
    , _ports :: [ContainerPort]
    , _env :: [EnvVar]
    , _resources :: ResourceRequirements
    , _volumeMounts :: [VolumeMount]
    , _livenessProbe :: Probe
    , _readinessProbe :: Probe
    , _lifecycle :: Lifecycle
    , _terminationMessagePath :: Text
    , _imagePullPolicy :: Text
    , _securityContext :: SecurityContext
    , _stdin :: Bool
    , _stdinOnce :: Bool
    , _tty :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''Container

instance FromJSON Container
instance ToJSON Container
instance Arbitrary Container where
    arbitrary = Container <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
