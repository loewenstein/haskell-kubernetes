{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Unversioned.Status
    ( Status (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.Unversioned.ListMeta
import Model.Unversioned.StatusDetails


data Status = Status
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ListMeta
    , _status :: Text
    , _message :: Text
    , _reason :: Text
    , _details :: StatusDetails
    , _code :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''Status

instance FromJSON Status
instance ToJSON Status
instance Arbitrary Status where
    arbitrary = Status <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
