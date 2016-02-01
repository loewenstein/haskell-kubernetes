{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Unversioned.Patch
    ( Patch (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Patch = Patch deriving (Show, Eq, Generic)
makeLenses ''Patch

instance FromJSON Patch
instance ToJSON Patch
instance Arbitrary Patch where
    arbitrary = return Patch
