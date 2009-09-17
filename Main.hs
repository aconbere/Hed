module Main (main) where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import IO
import System

import Hed

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

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optFile = Nothing } = opts

    case optFile opts of
        Nothing -> hed "This helps me know how things are initialized \n\n help"
        Just f -> do
            f_contents <- readFile f
            hed f_contents
