{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.NamespaceStatus
    ( NamespaceStatus (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data NamespaceStatus = NamespaceStatus
    { _phase :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''NamespaceStatus

instance FromJSON NamespaceStatus
instance ToJSON NamespaceStatus
instance Arbitrary NamespaceStatus where
    arbitrary = NamespaceStatus <$> arbitrary
