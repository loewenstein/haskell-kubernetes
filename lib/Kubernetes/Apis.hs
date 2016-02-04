-- This source code is distributed under the terms of a MIT license,
-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Kubernetes.Apis (
      api
    , API
    ) where

import Data.Proxy
import Kubernetes.Api.ApivApi (ApivApi)

type API = ApivApi

api :: Proxy API
api = Proxy
