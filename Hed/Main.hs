module Hed.Main (hed) where

import System.Environment
import System.Console.GetOpt
import Hed.VTY

hed text =
    window text
