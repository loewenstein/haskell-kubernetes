{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.PodTemplate
    ( PodTemplate (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.PodTemplateSpec


data PodTemplate = PodTemplate
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _template :: PodTemplateSpec
    } deriving (Show, Eq, Generic)
makeLenses ''PodTemplate

instance FromJSON PodTemplate
instance ToJSON PodTemplate
instance Arbitrary PodTemplate where
    arbitrary = PodTemplate <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
