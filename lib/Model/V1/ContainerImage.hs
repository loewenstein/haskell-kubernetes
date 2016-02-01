{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.ContainerImage
    ( ContainerImage (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ContainerImage = ContainerImage
    { _repoTags :: [Text]
    , _size :: Integer
    } deriving (Show, Eq, Generic)
makeLenses ''ContainerImage

instance FromJSON ContainerImage
instance ToJSON ContainerImage
instance Arbitrary ContainerImage where
    arbitrary = ContainerImage <$> arbitrary <*> arbitrary
