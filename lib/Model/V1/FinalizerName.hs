{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.FinalizerName
    ( FinalizerName (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import GHC.Generics
import Test.QuickCheck

data FinalizerName = FinalizerName deriving (Show, Eq, Generic)
makeLenses ''FinalizerName

instance FromJSON FinalizerName
instance ToJSON FinalizerName
instance Arbitrary FinalizerName where
    arbitrary = return FinalizerName
