{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodList
    ( PodList (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Pod
import Model.Unversioned.ListMeta


data PodList = PodList
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: Unversioned.ListMeta
    , _items :: [Pod]
    } deriving (Show, Eq, Generic)
makeLenses ''PodList

instance FromJSON PodList
instance ToJSON PodList
instance Arbitrary PodList where
    arbitrary = PodList <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
