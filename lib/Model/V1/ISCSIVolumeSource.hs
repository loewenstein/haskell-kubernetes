{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ISCSIVolumeSource
    ( ISCSIVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ISCSIVolumeSource = ISCSIVolumeSource
    { _targetPortal :: Text
    , _iqn :: Text
    , _lun :: Integer
    , _iscsiInterface :: Text
    , _fsType :: Text
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''ISCSIVolumeSource

instance FromJSON ISCSIVolumeSource
instance ToJSON ISCSIVolumeSource
instance Arbitrary ISCSIVolumeSource where
    arbitrary = ISCSIVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
