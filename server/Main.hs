-- This source code is distributed under the terms of a MIT license,
-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

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
