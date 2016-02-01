{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodTemplateSpec
    ( PodTemplateSpec (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.PodSpec


data PodTemplateSpec = PodTemplateSpec
    { _metadata :: ObjectMeta
    , _spec :: PodSpec
    } deriving (Show, Eq, Generic)
makeLenses ''PodTemplateSpec

instance FromJSON PodTemplateSpec
instance ToJSON PodTemplateSpec
instance Arbitrary PodTemplateSpec where
    arbitrary = PodTemplateSpec <$> arbitrary <*> arbitrary
