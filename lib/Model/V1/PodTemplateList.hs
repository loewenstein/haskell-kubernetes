{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodTemplateList
    ( PodTemplateList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.PodTemplate
import Model.Unversioned.ListMeta


data PodTemplateList = PodTemplateList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _items :: [PodTemplate]
    } deriving (Show, Eq, Generic)
makeLenses ''PodTemplateList

instance FromJSON PodTemplateList
instance ToJSON PodTemplateList
instance Arbitrary PodTemplateList where
    arbitrary = PodTemplateList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
