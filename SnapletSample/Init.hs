{-# LANGUAGE OverloadedStrings #-}
module SnapletSample.Init (appInit) where

import Snap
import Snap.Util.FileServe
import Snap.Snaplet.Heist
import Snap.Snaplet.Config

import SnapletSample.Options
import SnapletSample.Routes (site) 
import SnapletSample.State (SampleState(SampleStateT)) 

appInit :: SnapletOptions -> SnapletInit SampleState SampleState
appInit config = makeSnaplet "SampleSnaplet" "My example Snaplet" Nothing $ do
    addRoutes site
    wrapSite (<|> (dir "static" (serveDirectory ".")))
    return $ SampleStateT config
