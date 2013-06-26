{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid ( mempty )
import Options.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import App.AppState
import Routes.Routes

main :: IO ()
main = do
    config <- commandLineConfig mempty
    simpleHttpServe (config :: Config AppStateT ()) site
