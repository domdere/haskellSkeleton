{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SnapletSample.State where

import Control.Lens.TH
import qualified Data.ByteString.Char8 as BS
import Snap
import Snap.Snaplet.Config

import SnapletSample.Options

data SampleState = SampleStateT { getCommandLineOptions :: SnapletOptions }

makeLenses ''SampleState
