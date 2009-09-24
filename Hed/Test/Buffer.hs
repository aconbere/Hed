module Hed.Test.Buffer where

import Hed.Buffer
import Test.QuickCheck

instance Arbitrary EditBuffer where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        cont <- arbitrary
        return (mkBuffer x y cont)

prop_boundedValue x y =
    boundedValue (abs x) y <= abs x

prop_mkBuffer x y cont =
    contents (mkBuffer x y cont) == cont

prop_lineCount buf =
    lineCount buf == length (lines (contents buf))

prop_insertCharMove buf ch
    | ch == '\n' = snd (cursor (insertChar buf ch)) == snd (cursor buf) + 1
    | otherwise = fst (cursor (insertChar buf ch)) == fst (cursor buf) + 1

prop_insertChar buf ch =
    deleteCharBackward (insertChar buf ch) == buf

prop_deleteCharForward buf ch =
    deleteCharForward (moveLeft (insertChar buf ch)) == buf

prop_split buf =
    x ++ y == contents buf
    where (x,y) = split buf
