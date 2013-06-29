{-# LANGUAGE TemplateHaskell OverloadedStrings #-}
module SnapletSample.State where

import Control.Len.TH
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Maybe
import Snap
import Snap.Snaplet.Heist

data SampleState = SampleState {_someBS :: IORef BS.ByteString}

makeLenses ''SampleState
