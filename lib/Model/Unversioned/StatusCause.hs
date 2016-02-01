{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Unversioned.StatusCause
    ( StatusCause (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data StatusCause = StatusCause
    { _reason :: Text
    , _message :: Text
    , _field :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''StatusCause

instance FromJSON StatusCause
instance ToJSON StatusCause
instance Arbitrary StatusCause where
    arbitrary = StatusCause <$> arbitrary <*> arbitrary <*> arbitrary
