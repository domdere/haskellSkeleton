{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module SnapletSample.State where

import Control.Lens.TH
import qualified Data.ByteString.Char8 as BS
import Snap
import Snap.Snaplet.Config

data SampleState = SampleStateT { getAppConfig :: (Config Snap AppConfig) }

makeLenses ''SampleState
