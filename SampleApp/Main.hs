{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Writer (liftIO)
import Data.Text (append, pack)
import Options.Applicative
import System.IO
import System.Log.FastLogger (mkLogger, Logger, loggerFlush)

import Options.LogOptions
import SampleApp.CommandLineOptions
import System.Logging
import TestLib.TestModule as TestLib
import TestFFI.TestWrapper as Wrapped

-- | Entry point for the test application

loggerFunc :: LoggingT IO ()
loggerFunc = do
    liftIO $ putStrLn "Inside Logging Test Function"
    $(logInfo) "Demo log line"
    liftIO $ putStrLn "Leaving Logging Test Function"
    return ()

parseOpts :: IO CommandLineOptions
parseOpts = execParser options
    where
        options = info (helper <*> optparser)
            ( fullDesc    
            <> progDesc "Print your program info here"
            <> header "header goes here" )

logOptions :: CommandLineOptions -> LoggingT IO ()
logOptions options = do
    $(logInfo) ("Command Line Options: " `append` ((pack . show) options))

main :: IO ()
main = do
    options <- parseOpts
    putStrLn $ show options
    logFile <- openFile ((logfile . logOpts) options) AppendMode 
    logger <- mkLogger False logFile    
    runLoggingT (logOptions options) $ monadLoggerFunction logger LevelInfo
    putStr $ show $ TestLib.testFunction 2
    Wrapped.hello_world
    runLoggingT loggerFunc $ monadLoggerFunction logger LevelInfo 
    putStrLn "The End."
    loggerFlush logger
    hClose logFile
