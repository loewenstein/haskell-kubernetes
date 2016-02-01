{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeStatus
    ( PersistentVolumeStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data PersistentVolumeStatus = PersistentVolumeStatus
    { _phase :: Text
    , _message :: Text
    , _reason :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeStatus

instance FromJSON PersistentVolumeStatus
instance ToJSON PersistentVolumeStatus
instance Arbitrary PersistentVolumeStatus where
    arbitrary = PersistentVolumeStatus <$> arbitrary <*> arbitrary <*> arbitrary
