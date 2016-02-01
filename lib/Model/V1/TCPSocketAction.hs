{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.TCPSocketAction
    ( TCPSocketAction (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()


data TCPSocketAction = TCPSocketAction
    { _port :: Text
    } deriving (Show, Eq, Generic)
makeLenses ''TCPSocketAction

instance FromJSON TCPSocketAction
instance ToJSON TCPSocketAction
instance Arbitrary TCPSocketAction where
    arbitrary = TCPSocketAction <$> arbitrary
