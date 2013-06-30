{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid ( mempty )
import Options.Applicative
import Snap

import SnapletSample.Init (appInit)
import SnapletSample.State (SampleState(SampleStateT))

main :: IO ()
main = do
    config <- commandLineConfig mempty
    serveSnaplet config (appInit config)
