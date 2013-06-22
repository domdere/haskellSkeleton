{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Monad.Logger
import Control.Monad.Writer
import Control.Monad.Trans.Control
import System.IO
import System.Log.FastLogger (mkLogger, Logger, loggerFlush)

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

main :: IO ()
main = do
    logFile <- openFile "test.log" AppendMode 
    logger <- mkLogger False logFile    
    putStr $ show $ TestLib.testFunction 2
    Wrapped.hello_world
    runLoggingT loggerFunc $ monadLoggerFunction logger LevelInfo 
    putStrLn "The End."
    loggerFlush logger
    hClose logFile
