{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Options.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Routes.Routes

main :: IO ()
main = quickHttpServe site
