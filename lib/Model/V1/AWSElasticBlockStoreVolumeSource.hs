{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.AWSElasticBlockStoreVolumeSource
    ( AWSElasticBlockStoreVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data AWSElasticBlockStoreVolumeSource = AWSElasticBlockStoreVolumeSource
    { _volumeID :: Text
    , _fsType :: Text
    , _partition :: Integer
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''AWSElasticBlockStoreVolumeSource

instance FromJSON AWSElasticBlockStoreVolumeSource
instance ToJSON AWSElasticBlockStoreVolumeSource
instance Arbitrary AWSElasticBlockStoreVolumeSource where
    arbitrary = AWSElasticBlockStoreVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
