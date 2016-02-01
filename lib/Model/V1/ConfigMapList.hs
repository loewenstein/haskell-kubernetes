{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ConfigMapList
    ( ConfigMapList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ConfigMap
import Model.Unversioned.ListMeta


data ConfigMapList = ConfigMapList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: Unversioned.ListMeta
    , _items :: [ConfigMap]
    } deriving (Show, Eq, Generic)
makeLenses ''ConfigMapList

instance FromJSON ConfigMapList
instance ToJSON ConfigMapList
instance Arbitrary ConfigMapList where
    arbitrary = ConfigMapList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
