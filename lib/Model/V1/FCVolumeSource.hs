{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.FCVolumeSource
    ( FCVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data FCVolumeSource = FCVolumeSource
    { _targetWWNs :: [Text]
    , _lun :: Integer
    , _fsType :: Text
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''FCVolumeSource

instance FromJSON FCVolumeSource
instance ToJSON FCVolumeSource
instance Arbitrary FCVolumeSource where
    arbitrary = FCVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
