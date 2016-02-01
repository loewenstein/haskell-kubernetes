{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EnvVarSource
    ( EnvVarSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ConfigMapKeySelector
import Model.V1.ObjectFieldSelector
import Model.V1.SecretKeySelector


data EnvVarSource = EnvVarSource
    { _fieldRef :: ObjectFieldSelector
    , _configMapKeyRef :: ConfigMapKeySelector
    , _secretKeyRef :: SecretKeySelector
    } deriving (Show, Eq, Generic)
makeLenses ''EnvVarSource

instance FromJSON EnvVarSource
instance ToJSON EnvVarSource
instance Arbitrary EnvVarSource where
    arbitrary = EnvVarSource <$> arbitrary <*> arbitrary <*> arbitrary
