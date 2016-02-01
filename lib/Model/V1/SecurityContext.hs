{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.SecurityContext
    ( SecurityContext (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.Capabilities
import Model.V1.SELinuxOptions


data SecurityContext = SecurityContext
    { _capabilities :: Capabilities
    , _privileged :: Bool
    , _seLinuxOptions :: SELinuxOptions
    , _runAsUser :: Integer
    , _runAsNonRoot :: Bool
    } deriving (Show, Eq, Generic)
makeLenses ''SecurityContext

instance FromJSON SecurityContext
instance ToJSON SecurityContext
instance Arbitrary SecurityContext where
    arbitrary = SecurityContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
