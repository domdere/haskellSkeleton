{-# LANGUAGE OverloadedStrings #-}
module SnapletSample.Handlers where

import Snap.Core

echoHandler :: (MonadSnap m) => m ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
