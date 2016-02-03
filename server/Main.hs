{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Kubernetes.Apis
import Servant
import Servant.Mock
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 8080 $ serve api (mock api)
