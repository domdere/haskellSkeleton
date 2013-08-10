module SnapletSample.Options 
    (   defaultOptions
    ,   sampleOptionDescrs
    ,   SnapletOptions
    ) where

import Data.Function
import Data.Monoid
import Snap
import Snap.Http.Server.Config
import System.Console.GetOpt

data SnapletOptions = SnapletOptions 
    {   dbport :: Maybe Int
    ,   dbuser :: Maybe String
    }

defaultOptions :: SnapletOptions
defaultOptions = SnapletOptions
    {   dbport = Nothing
    ,   dbuser = Nothing 
    }

instance Monoid SnapletOptions where
    mempty      = defaultOptions
    mappend x y = SnapletOptions
        {   dbport = ov dbport
        ,   dbuser = ov dbuser 
        }
        where
            ov f = getLast $! (mappend `on` (Last . f)) x y

fromSnapletOptions :: SnapletOptions -> Config Snap SnapletOptions
fromSnapletOptions options = setOther options defaultConfig

fromPort :: String -> Maybe (Config Snap SnapletOptions)
fromPort port = (Just . fromSnapletOptions) (defaultOptions
    {   dbport = Just (read port)
    })

fromUser :: String -> Maybe (Config Snap SnapletOptions)
fromUser username = (Just . fromSnapletOptions) (defaultOptions
    {   dbuser = Just username
    })

sampleOptionDescrs :: [OptDescr (Maybe (Config Snap SnapletOptions))]
sampleOptionDescrs = 
    [   Option ['p'] ["dbport"] (ReqArg fromPort "PORT") "port"
    ,   Option ['u'] ["dbuser"] (ReqArg fromUser "USERNAME") "username"
    ]

