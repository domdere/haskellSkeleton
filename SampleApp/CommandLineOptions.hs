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

logFileOpt :: Parser FilePath
logFileOpt = strOption
    (long "logfile"
    <> metavar "FILE"
    <> help "Path to Log File, default=['./test.log']")

optionalLogFileOpt :: Parser (Maybe FilePath)
optionalLogFileOpt = optional logFileOpt

logLevelStrOption :: Parser String
logLevelStrOption = strOption
    (long "loglevel"
    <> metavar "LOGLEVEL"
    <> help "The minimum log level that gets logged, one of: LogDebug, LogInfo, LogWarn, LevelError, default=['LogInfo']")

logLevelOpt :: Parser LogLevel
logLevelOpt = read `fmap` logLevelStrOption

optionalLogLevelOpt :: Parser (Maybe LogLevel)
optionalLogLevelOpt = optional logLevelOpt
