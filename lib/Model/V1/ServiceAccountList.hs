{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ServiceAccountList
    ( ServiceAccountList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ServiceAccount
import Model.Unversioned.ListMeta


data ServiceAccountList = ServiceAccountList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [ServiceAccount]
    } deriving (Show, Eq, Generic)
makeLenses ''ServiceAccountList

instance FromJSON ServiceAccountList
instance ToJSON ServiceAccountList
instance Arbitrary ServiceAccountList where
    arbitrary = ServiceAccountList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
