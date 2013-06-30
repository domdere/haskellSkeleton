{-# LANGUAGE OverloadedStrings #-}
module SnapletSample.Init (appInit) where

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Config

import SnapletSample.Routes (site) 
import SnapletSample.State (SampleState(SampleStateT)) 

appInit :: Config Snap AppConfig -> SnapletInit SampleState SampleState
appInit config = makeSnaplet "SampleSnaplet" "My example Snaplet" Nothing $ do
    addRoutes site
    return $ SampleStateT config
