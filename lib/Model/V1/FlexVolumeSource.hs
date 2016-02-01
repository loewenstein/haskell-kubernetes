{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.FlexVolumeSource
    ( FlexVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Any
import Model.V1.LocalObjectReference


data FlexVolumeSource = FlexVolumeSource
    { _driver :: Text
    , _fsType :: Text
    , _secretRef :: LocalObjectReference
    , _readOnly :: Bool
    , _options :: Any
    } deriving (Show, Eq, Generic)
makeLenses ''FlexVolumeSource

instance FromJSON FlexVolumeSource
instance ToJSON FlexVolumeSource
instance Arbitrary FlexVolumeSource where
    arbitrary = FlexVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
