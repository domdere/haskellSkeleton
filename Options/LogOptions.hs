
module Options.LogOptions (LogOptions, logfile, loglevel, mkLogOptions, optionalLogFileOpt, optionalLogLevelOpt) where

import Control.Monad.Logger (LogLevel)
import Options.Applicative

data LogOptions = LogOptions {
    logfile :: FilePath,
    loglevel :: LogLevel
} deriving (Show, Eq)

optionalMaybeWithDefault :: a -> Maybe a -> a
optionalMaybeWithDefault defaultVal Nothing = defaultVal
optionalMaybeWithDefault _ (Just x)         = x

mkLogOptions :: FilePath -> LogLevel -> Maybe FilePath -> Maybe LogLevel -> LogOptions
mkLogOptions defaultLogPath defaultLogLevel logPath logLevel = LogOptions 
    (optionalMaybeWithDefault defaultLogPath logPath) 
    (optionalMaybeWithDefault defaultLogLevel logLevel)

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
