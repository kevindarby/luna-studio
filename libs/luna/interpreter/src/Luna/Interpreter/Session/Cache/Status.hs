---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Status where

import Flowbox.Prelude



data CacheStatus = Ready
                 | Modified
                 deriving (Show, Eq)
