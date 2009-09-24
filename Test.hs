module Hed.Test (
    module Test.Buffer
    ) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.Buffer

main = defaultMain tests

tests = [
    testGroup "Buffer" [ testProperty "boundedValue" Test.Buffer.prop_boundedValue
                       , testProperty "mkBuffer" Test.Buffer.prop_mkBuffer
                       , testProperty "insertCharMove" Test.Buffer.prop_insertCharMove
                       , testProperty "insertChar" Test.Buffer.prop_insertChar
                       , testProperty "deleteCharForward" Test.Buffer.prop_deleteCharForward
                       ]
    ]

