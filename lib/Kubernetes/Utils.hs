-- This source code is distributed under the terms of a BSD license,
-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Kubernetes.Utils where

import GHC.Generics
import Servant.API
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.QuickCheck

instance FromText [String] where
    fromText = Just . splitOn "," . T.unpack

instance ToText [String] where
    toText = T.pack . intercalate ","

lkp inputs l = case lookup l inputs of
        Nothing -> Left $ "label " ++ T.unpack l ++ " not found"
        Just v  -> Right $ read (T.unpack v)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
