{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App.AppState where

import Control.Applicative ( Applicative(), Alternative() )
import Control.Monad.CatchIO ( MonadCatchIO() )
import Control.Monad.IO.Class ()
import Control.Monad.State
import Snap.Core

data AppState = EmptyState | AppState deriving (Show, Eq)


newtype AppStateT a = AppStateT { 
    runAppStateT :: StateT AppState Snap a 
} deriving (Monad, MonadIO, MonadCatchIO, MonadPlus, Functor, Applicative, Alternative, MonadSnap)

