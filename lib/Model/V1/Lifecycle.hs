{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Lifecycle
    ( Lifecycle (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Handler


data Lifecycle = Lifecycle
    { _postStart :: Handler
    , _preStop :: Handler
    } deriving (Show, Eq, Generic)
makeLenses ''Lifecycle

instance FromJSON Lifecycle
instance ToJSON Lifecycle
instance Arbitrary Lifecycle where
    arbitrary = Lifecycle <$> arbitrary <*> arbitrary
