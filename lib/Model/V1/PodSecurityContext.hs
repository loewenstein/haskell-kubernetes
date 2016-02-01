{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodSecurityContext
    ( PodSecurityContext (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.SELinuxOptions


data PodSecurityContext = PodSecurityContext
    { _seLinuxOptions :: SELinuxOptions
    , _runAsUser :: Integer
    , _runAsNonRoot :: Bool
    , _supplementalGroups :: [Integer]
    , _fsGroup :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''PodSecurityContext

instance FromJSON PodSecurityContext
instance ToJSON PodSecurityContext
instance Arbitrary PodSecurityContext where
    arbitrary = PodSecurityContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
