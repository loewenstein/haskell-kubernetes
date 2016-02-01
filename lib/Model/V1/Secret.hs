{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Secret
    ( Secret (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.ObjectMeta


data Secret = Secret
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _data_ :: Value
    , _type_ :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''Secret

instance FromJSON Secret
instance ToJSON Secret
instance Arbitrary Secret where
    arbitrary = Secret <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
