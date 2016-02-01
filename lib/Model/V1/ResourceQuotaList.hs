{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ResourceQuotaList
    ( ResourceQuotaList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ResourceQuota
import Model.Unversioned.ListMeta


data ResourceQuotaList = ResourceQuotaList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [ResourceQuota]
    } deriving (Show, Eq, Generic)
makeLenses ''ResourceQuotaList

instance FromJSON ResourceQuotaList
instance ToJSON ResourceQuotaList
instance Arbitrary ResourceQuotaList where
    arbitrary = ResourceQuotaList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
