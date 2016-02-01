{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ComponentStatus
    ( ComponentStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ComponentCondition
import Model.V1.ObjectMeta


data ComponentStatus = ComponentStatus
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _conditions :: [ComponentCondition]
    } deriving (Show, Eq, Generic)
makeLenses ''ComponentStatus

instance FromJSON ComponentStatus
instance ToJSON ComponentStatus
instance Arbitrary ComponentStatus where
    arbitrary = ComponentStatus <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
