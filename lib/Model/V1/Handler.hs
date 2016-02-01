{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Model.V1.Handler
    ( Handler (..)
    ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Model.V1.ExecAction
import Model.V1.HTTPGetAction
import Model.V1.TCPSocketAction


data Handler = Handler
    { _exec :: ExecAction
    , _httpGet :: HTTPGetAction
    , _tcpSocket :: TCPSocketAction
    } deriving (Show, Eq, Generic)
makeLenses ''Handler

instance FromJSON Handler
instance ToJSON Handler
instance Arbitrary Handler where
    arbitrary = Handler <$> arbitrary <*> arbitrary <*> arbitrary
