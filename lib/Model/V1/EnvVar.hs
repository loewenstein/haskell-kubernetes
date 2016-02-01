{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.EnvVar
    ( EnvVar (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.EnvVarSource


data EnvVar = EnvVar
    { _name :: Text
    , _value :: Text
    , _valueFrom :: EnvVarSource
    } deriving (Show, Eq, Generic)
makeLenses ''EnvVar

instance FromJSON EnvVar
instance ToJSON EnvVar
instance Arbitrary EnvVar where
    arbitrary = EnvVar <$> arbitrary <*> arbitrary <*> arbitrary
