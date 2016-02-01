{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerStateWaiting
    ( ContainerStateWaiting (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ContainerStateWaiting = ContainerStateWaiting
    { _reason :: Text
    , _message :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerStateWaiting

instance FromJSON ContainerStateWaiting
instance ToJSON ContainerStateWaiting
instance Arbitrary ContainerStateWaiting where
    arbitrary = ContainerStateWaiting <$> arbitrary <*> arbitrary
