{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Options.Applicative
import Snap
import Snap.Http.Server.Config

import SnapletSample.Init (appInit)
import SnapletSample.Options
import SnapletSample.State (SampleState(SampleStateT))

main :: IO ()
main = 
    let
        getOptions = (fromMaybe defaultOptions) . getOther
        optionDescrs = (optDescrs defaultConfig) ++ sampleOptionDescrs
    
    in do
        snapletConfig <- extendedCommandLineConfig optionDescrs mappend defaultConfig
        -- haven't figured out how to get both 
        -- the snapletConfig and AppConfig
        -- separately without getting the rest together
        appConfig <- commandLineConfig mempty 
        serveSnaplet appConfig (appInit (getOptions snapletConfig))
