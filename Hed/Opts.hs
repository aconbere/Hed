module Hed.Opts where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import IO
import System

version = "0.01"

data Options = Options { optFile :: Maybe String} deriving (Show)
startOptions = Options { optFile = Nothing }

options =
    [ Option ['i'] ["input"]
        (ReqArg
            (\arg opt -> return opt { optFile = Just arg })
            "FILE")
        "Input file"
 
    , Option ['V'] ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (concat ["Version: ", version])
                exitWith ExitSuccess))
        "Print version"
 
    , Option ['h'] ["help"]
        (NoArg
            (\_ -> do
    	        prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]
