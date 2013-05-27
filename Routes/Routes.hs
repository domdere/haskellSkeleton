{-# LANGUAGE OverloadedStrings #-}
module Routes.Routes where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server 

import Handlers.Echo

site :: Snap()
site = 
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar"),
        ("echo/:echoparam", echoHandler)] <|>
    dir "static" (serveDirectory ".")
