{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Capabilities
    ( Capabilities (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Capability


data Capabilities = Capabilities
    { _add :: [Capability]
    , _drop :: [Capability]
    } deriving (Show, Eq, Generic)
makeLenses ''Capabilities

instance FromJSON Capabilities
instance ToJSON Capabilities
instance Arbitrary Capabilities where
    arbitrary = Capabilities <$> arbitrary <*> arbitrary
