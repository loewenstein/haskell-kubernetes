{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Unversioned.StatusDetails
    ( StatusDetails (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.Unversioned.StatusCause


data StatusDetails = StatusDetails
    { _name :: Text
    , _group :: Text
    , _kind :: Text
    , _causes :: [StatusCause]
    , _retryAfterSeconds :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''StatusDetails

instance FromJSON StatusDetails
instance ToJSON StatusDetails
instance Arbitrary StatusDetails where
    arbitrary = StatusDetails <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
