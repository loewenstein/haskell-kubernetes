{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.RBDVolumeSource
    ( RBDVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.LocalObjectReference


data RBDVolumeSource = RBDVolumeSource
    { _monitors :: [Text]
    , _image :: Text
    , _fsType :: Text
    , _pool :: Text
    , _user :: Text
    , _keyring :: Text
    , _secretRef :: LocalObjectReference
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''RBDVolumeSource

instance FromJSON RBDVolumeSource
instance ToJSON RBDVolumeSource
instance Arbitrary RBDVolumeSource where
    arbitrary = RBDVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
