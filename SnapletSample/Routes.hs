{-# LANGUAGE OverloadedStrings #-}
module SnapletSample.Routes where

import qualified Data.ByteString.Char8 as BS
import Snap.Core

import SnapletSample.Handlers (echoHandler)

site :: (MonadSnap m) => [(BS.ByteString, m ())]
site = [("foo", writeBS "bar"), ("echo/:echoparam", echoHandler)]
