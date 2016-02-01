{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.GCEPersistentDiskVolumeSource
    ( GCEPersistentDiskVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data GCEPersistentDiskVolumeSource = GCEPersistentDiskVolumeSource
    { _pdName :: Text
    , _fsType :: Text
    , _partition :: Integer
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''GCEPersistentDiskVolumeSource

instance FromJSON GCEPersistentDiskVolumeSource
instance ToJSON GCEPersistentDiskVolumeSource
instance Arbitrary GCEPersistentDiskVolumeSource where
    arbitrary = GCEPersistentDiskVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
