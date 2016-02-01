{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Node
    ( Node (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.NodeSpec
import Model.V1.NodeStatus
import Model.V1.ObjectMeta


data Node = Node
    { _kind :: Text
    , _apiVersion :: Text
    , _metadata :: ObjectMeta
    , _spec :: NodeSpec
    , _status :: NodeStatus
    } deriving (Show, Eq, Generic)
makeLenses ''Node

instance FromJSON Node
instance ToJSON Node
instance Arbitrary Node where
    arbitrary = Node <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
