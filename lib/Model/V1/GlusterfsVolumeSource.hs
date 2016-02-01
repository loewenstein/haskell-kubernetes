{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.GlusterfsVolumeSource
    ( GlusterfsVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data GlusterfsVolumeSource = GlusterfsVolumeSource
    { _endpoints :: Text
    , _path :: Text
    , _readOnly :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''GlusterfsVolumeSource

instance FromJSON GlusterfsVolumeSource
instance ToJSON GlusterfsVolumeSource
instance Arbitrary GlusterfsVolumeSource where
    arbitrary = GlusterfsVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
