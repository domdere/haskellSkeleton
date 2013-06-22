{-# LANGUAGE OverloadedStrings #-}

module System.Logging where

import Control.Monad.Logger
import qualified Data.Text as Text
import Language.Haskell.TH
import System.Log.FastLogger

-- | Takes a desired loglevel and a log line level and checks to see if that line should be logged
doesLog :: LogLevel -> LogLevel -> Bool
doesLog desiredLevel logLineLevel = logLineLevel >= desiredLevel

formatSource :: LogSource -> LogStr
formatSource source
    | Text.null source  = LS ""
    | otherwise         = LS $ Text.unpack source

formatLogLevel :: LogLevel -> LogStr
formatLogLevel (LevelOther text)    = LS $ Text.unpack text
formatLogLevel loglevel             = LS $ show loglevel

formatFileLocation :: Loc -> String
formatFileLocation (Loc filename package moduleName start end) = package ++ ":" ++ moduleName ++ ":" ++ filename ++ ":" ++ ((show . fst) start) ++ ((show . snd) start)

formatLogging :: IO ZonedDate -> Loc -> LogSource -> LogLevel -> LogStr -> IO [LogStr]
formatLogging dateAction codeLoc source loglevel msg =  do
    today <- dateAction
    return [LB today, LB " ", formatSource source, LB " (", formatLogLevel loglevel, LB "): ", msg, LB " (", LS (formatFileLocation codeLoc), LB ")\n"]

type MonadLoggerFunction = (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

monadLoggerFunction :: Logger -> LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
monadLoggerFunction logger desiredLogLevel loc source loglevel mesg
    | not (doesLog desiredLogLevel loglevel) = return ()
    | otherwise = do
        logStrs <- formatLogging (loggerDate logger) loc source loglevel mesg
        loggerPutStr logger logStrs
