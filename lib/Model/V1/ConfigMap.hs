{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ConfigMap
    ( ConfigMap (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.ObjectMeta


data ConfigMap = ConfigMap
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _data_ :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''ConfigMap

instance FromJSON ConfigMap
instance ToJSON ConfigMap
instance Arbitrary ConfigMap where
    arbitrary = ConfigMap <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
