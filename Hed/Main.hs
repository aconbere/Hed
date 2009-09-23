module Hed.Main (hed) where

import Hed.VTY
import Hed.Buffer

hed :: Maybe String -> String -> IO ()
hed f = start . Window f . mkBuffer 0 0
