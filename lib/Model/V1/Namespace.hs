{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Namespace
    ( Namespace (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.NamespaceSpec
import Model.V1.NamespaceStatus
import Model.V1.ObjectMeta


data Namespace = Namespace
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: NamespaceSpec
    , _status :: NamespaceStatus
    } deriving (Show, Eq, Generic)
makeLenses ''Namespace

instance FromJSON Namespace
instance ToJSON Namespace
instance Arbitrary Namespace where
    arbitrary = Namespace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
