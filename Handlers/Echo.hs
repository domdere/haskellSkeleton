{-# LANGUAGE OverloadedStrings #-}
module Handlers.Echo where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
