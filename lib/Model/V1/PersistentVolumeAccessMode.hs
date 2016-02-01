{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PersistentVolumeAccessMode
    ( PersistentVolumeAccessMode (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import GHC.Generics
import Test.QuickCheck

data PersistentVolumeAccessMode = PersistentVolumeAccessMode deriving (Show, Eq, Generic)
makeLenses ''PersistentVolumeAccessMode

instance FromJSON PersistentVolumeAccessMode
instance ToJSON PersistentVolumeAccessMode
instance Arbitrary PersistentVolumeAccessMode where
    arbitrary = return PersistentVolumeAccessMode
