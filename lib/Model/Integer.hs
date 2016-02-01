{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Integer
    ( Integer (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import GHC.Generics
import Test.QuickCheck


data Integer = Integer deriving (Show, Eq, Generic)
makeLenses ''Integer

instance FromJSON Integer
instance ToJSON Integer
instance Arbitrary Integer where
    arbitrary = Integer
