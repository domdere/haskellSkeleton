module SampleApp.CommandLineOptions (CommandLineOptions, logOpts, optparser) where

import Control.Monad.Logger (LogLevel (LevelDebug, LevelInfo, LevelWarn, LevelError) )
import Options.Applicative

import Options.LogOptions

data CommandLineOptions = CommandLineOptions {
    logOpts :: LogOptions
} deriving (Show, Eq)

mkCommandLineOptions :: FilePath -> LogLevel -> Maybe FilePath -> Maybe LogLevel -> CommandLineOptions
mkCommandLineOptions defaultFilePath defaultLogLevel filePath logLevel = CommandLineOptions $ mkLogOptions defaultFilePath defaultLogLevel filePath logLevel

optparser :: Parser CommandLineOptions
optparser = (mkCommandLineOptions "./test.log" LevelInfo)
    <$> optionalLogFileOpt
    <*> optionalLogLevelOpt
