{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.Unversioned.ListMeta
    ( ListMeta (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data ListMeta = ListMeta
    { _selfLink :: Text
    , _resourceVersion :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''ListMeta

instance FromJSON ListMeta
instance ToJSON ListMeta
instance Arbitrary ListMeta where
    arbitrary = ListMeta <$> arbitrary <*> arbitrary
