{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NamespaceList
    ( NamespaceList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Namespace
import Model.Unversioned.ListMeta


data NamespaceList = NamespaceList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [Namespace]
    } deriving (Show, Eq, Generic)
makeLenses ''NamespaceList

instance FromJSON NamespaceList
instance ToJSON NamespaceList
instance Arbitrary NamespaceList where
    arbitrary = NamespaceList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
