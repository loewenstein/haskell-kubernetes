{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerStateTerminated
    ( ContainerStateTerminated (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ContainerStateTerminated = ContainerStateTerminated
    { _exitCode :: Integer
    , _signal :: Integer
    , _reason :: Text
    , _message :: Text
    , _startedAt :: Text
    , _finishedAt :: Text
    , _containerID :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerStateTerminated

instance FromJSON ContainerStateTerminated
instance ToJSON ContainerStateTerminated
instance Arbitrary ContainerStateTerminated where
    arbitrary = ContainerStateTerminated <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
