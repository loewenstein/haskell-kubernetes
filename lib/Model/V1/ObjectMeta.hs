{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ObjectMeta
    ( ObjectMeta (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any


data ObjectMeta = ObjectMeta
    { _name :: Text
    , _generateName :: Text
    , _namespace :: Text
    , _selfLink :: Text
    , _uid :: Text
    , _resourceVersion :: Text
    , _generation :: Integer
    , _creationTimestamp :: Text
    , _deletionTimestamp :: Text
    , _deletionGracePeriodSeconds :: Integer
    , _labels :: Value
    , _annotations :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''ObjectMeta

instance FromJSON ObjectMeta
instance ToJSON ObjectMeta
instance Arbitrary ObjectMeta where
    arbitrary = ObjectMeta <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
