module Hed.Test (
    module Hed.Test.Buffer
    ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Hed.Test.Buffer
import Test.QuickCheck

main = defaultMain tests

tests = [
    testGroup "Buffer" [ testProperty "boundedValue" prop_boundedValue
                       , testProperty "mkBuffer" prop_mkBuffer
                       , testProperty "insertCharMove" prop_insertCharMove
                       , testProperty "insertChar" prop_insertChar
                       , testProperty "deleteCharForward" prop_deleteCharForward
                       ]
    ]

