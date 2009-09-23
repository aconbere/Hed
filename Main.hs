module Main (main) where

import System.Exit
import System.IO
import System.Console.GetOpt
import System.Environment

import Hed

version :: String
version = "0.01"

data Options = Options { optFile :: Maybe String} deriving (Show)

startOptions :: Options
startOptions = Options { optFile = Nothing }

options :: [OptDescr (Options -> IO Options)]
options =
    [Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (concat ["Version: ", version])
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
    	        prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonoptions, _) = getOpt RequireOrder options args
    foldl (>>=) (return startOptions) actions
    case nonoptions of
        [] -> hed Nothing ""
        (f:_) -> readFile f >>= hed (Just f)
