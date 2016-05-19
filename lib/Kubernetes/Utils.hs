-- This source code is distributed under the terms of a MIT license,
-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Kubernetes.Utils where

import           Control.Lens.TH (makeLenses)
import           Control.Monad   (mzero)
import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Aeson      as A
import           Data.Scientific (coefficient, scientific)
import qualified Data.Text       as T
import           Data.Text.Read  (decimal)
import           GHC.Generics
import           Servant.API
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

newtype IntegerOrText = IntegerOrText { unIntOrText :: Either Integer T.Text } deriving (Eq, Show, Generic)

makeLenses ''IntegerOrText

instance Arbitrary IntegerOrText where
  arbitrary = IntegerOrText <$> oneof [ Left <$> arbitrary , Right <$> arbitrary ]

instance FromJSON IntegerOrText where
  parseJSON (A.Number i) = return . IntegerOrText . Left . coefficient $ i
  parseJSON (A.String s) = return . IntegerOrText . Right $ s
  parseJSON _            = mzero

instance ToJSON IntegerOrText where
  toJSON (IntegerOrText (Left i))  = A.Number (scientific i 0)
  toJSON (IntegerOrText (Right s)) = A.String s

instance FromHttpApiData IntegerOrText where
  parseUrlPiece txt =
    case decimal txt of
      Right (i, t) | T.null t -> Right (IntegerOrText (Left i))
      _                       -> Right (IntegerOrText (Right txt))

instance ToHttpApiData IntegerOrText where
  toUrlPiece iot = case iot of
    IntegerOrText (Left i) -> T.pack (show i)
    IntegerOrText (Right t) -> t

lkp inputs l = case lookup l inputs of
        Nothing -> Left $ "label " ++ T.unpack l ++ " not found"
        Just v  -> Right $ read (T.unpack v)
