{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.GitRepoVolumeSource
    ( GitRepoVolumeSource (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data GitRepoVolumeSource = GitRepoVolumeSource
    { _repository :: Text
    , _revision :: Text
    , _directory :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''GitRepoVolumeSource

instance FromJSON GitRepoVolumeSource
instance ToJSON GitRepoVolumeSource
instance Arbitrary GitRepoVolumeSource where
    arbitrary = GitRepoVolumeSource <$> arbitrary <*> arbitrary <*> arbitrary
