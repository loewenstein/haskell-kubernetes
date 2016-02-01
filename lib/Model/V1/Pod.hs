{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Pod
    ( Pod (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.PodSpec
import Model.V1.PodStatus


data Pod = Pod
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: PodSpec
    , _status :: PodStatus
    } deriving (Show, Eq, Generic)
makeLenses ''Pod

instance FromJSON Pod
instance ToJSON Pod
instance Arbitrary Pod where
    arbitrary = Pod <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
