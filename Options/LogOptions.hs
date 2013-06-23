
module Options.LogOptions (LogOptions, logfile, loglevel, mkLogOptions) where

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

