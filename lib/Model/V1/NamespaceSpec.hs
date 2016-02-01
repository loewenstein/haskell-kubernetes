{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NamespaceSpec
    ( NamespaceSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.V1.FinalizerName


data NamespaceSpec = NamespaceSpec
    { _finalizers :: [FinalizerName]
    } deriving (Show, Eq, Generic)
makeLenses ''NamespaceSpec

instance FromJSON NamespaceSpec
instance ToJSON NamespaceSpec
instance Arbitrary NamespaceSpec where
    arbitrary = NamespaceSpec <$> arbitrary
