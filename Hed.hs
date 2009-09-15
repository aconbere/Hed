module Main where
import Opts
import System.Environment
import System.Console.GetOpt
import VTY

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optFile = Nothing } = opts

    case optFile opts of
        Nothing -> window "This helps me know how things are initialized \n\n help"
        Just f -> do
            f_contents <- readFile f
            window f_contents
