{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ObjectReference
    ( ObjectReference (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ObjectReference = ObjectReference
    { _kind :: Text
    , _namespace :: Text
    , _name :: Text
    , _uid :: Text
    , _apiVersion :: Text
    , _resourceVersion :: Text
    , _fieldPath :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ObjectReference

instance FromJSON ObjectReference
instance ToJSON ObjectReference
instance Arbitrary ObjectReference where
    arbitrary = ObjectReference <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
