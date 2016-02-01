{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.CephFSVolumeSource
    ( CephFSVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LocalObjectReference


data CephFSVolumeSource = CephFSVolumeSource
    { _monitors :: [Text]
    , _user :: Text
    , _secretFile :: Text
    , _secretRef :: LocalObjectReference
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''CephFSVolumeSource

instance FromJSON CephFSVolumeSource
instance ToJSON CephFSVolumeSource
instance Arbitrary CephFSVolumeSource where
    arbitrary = CephFSVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
