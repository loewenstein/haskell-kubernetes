{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Binding
    ( Binding (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ObjectMeta
import Model.V1.ObjectReference

data Binding = Binding
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _target :: ObjectReference
    } deriving (Show, Eq, Generic)
makeLenses ''Binding

instance FromJSON Binding
instance ToJSON Binding
instance Arbitrary Binding where
    arbitrary = Binding <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
