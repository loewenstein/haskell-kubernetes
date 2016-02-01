{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ResourceQuota
    ( ResourceQuota (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.ResourceQuotaSpec
import Model.V1.ResourceQuotaStatus


data ResourceQuota = ResourceQuota
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: ResourceQuotaSpec
    , _status :: ResourceQuotaStatus
    } deriving (Show, Eq, Generic)
makeLenses ''ResourceQuota

instance FromJSON ResourceQuota
instance ToJSON ResourceQuota
instance Arbitrary ResourceQuota where
    arbitrary = ResourceQuota <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
