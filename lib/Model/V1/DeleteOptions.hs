{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.DeleteOptions
    ( DeleteOptions (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data DeleteOptions = DeleteOptions
    { _kind :: Text
    , _apiVersion :: Text
    , _gracePeriodSeconds :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''DeleteOptions

instance FromJSON DeleteOptions
instance ToJSON DeleteOptions
instance Arbitrary DeleteOptions where
    arbitrary = DeleteOptions <$> arbitrary <*> arbitrary <*> arbitrary
