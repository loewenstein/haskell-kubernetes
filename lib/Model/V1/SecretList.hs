{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.SecretList
    ( SecretList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Secret
import Model.Unversioned.ListMeta


data SecretList = SecretList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: Unversioned.ListMeta
    , _items :: [Secret]
    } deriving (Show, Eq, Generic)
makeLenses ''SecretList

instance FromJSON SecretList
instance ToJSON SecretList
instance Arbitrary SecretList where
    arbitrary = SecretList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
