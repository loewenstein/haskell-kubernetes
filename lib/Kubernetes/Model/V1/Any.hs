{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Kubernetes.Model.V1.Any
    ( Any (..)
    ) where

import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (replicateM)
import           Data.Aeson
import qualified Data.HashMap.Strict       as HMap
import           Data.Text                 (Text)
import           Data.Vector               (fromList)
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

newtype Any =
  Any { _any :: Object
      } deriving (Show, Eq, FromJSON, ToJSON, Generic)
makeLenses ''Any

arbValue :: Gen Value
arbValue =
  let
    aValue :: Int -> Gen Value
    aValue 0 = oneof [ return Null
                     , Bool <$> arbitrary
                     , Number . fromInteger <$> arbitrary
                     , String <$> arbitrary
                     ]
    aValue n = do
      (Positive m) <- arbitrary
      let n' = quot n (m+1)
      oneof [ return Null
            , Bool <$> arbitrary
            , Number . fromInteger <$> arbitrary
            , String <$> arbitrary
            , Array . fromList <$> replicateM m (aValue n')
            ]
  in arbitrary >>= aValue

instance Arbitrary Any where
  arbitrary = Any . HMap.fromList <$> do
    keys <- listOf (arbitrary :: Gen Text)
    mapM (\k -> (,) k <$> arbValue) keys

