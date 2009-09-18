{- Copyright (c) 2009  Michael Feathers

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use,
   copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following
   conditions:
  
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
  
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE. -}

module Hed.Buffer
     ( empty
     , lineCount
     , insertChar, deleteCharForward, deleteCharBackward, replaceChar, insertString
     , insertLineAfter
     , deleteLine
     , moveLeft, moveRight, moveUp, moveDown
     , moveToHome, moveToEnd, moveToLine
     , moveToLineStart, moveToLineEnd
     , wordForward, wordBackward
     , initialBuffer
     , EditBuffer (..)
     ) where

import Data.Char

type Location = (Int, Int)
type TopLine = Int
data EditBuffer = EditBuffer { topLine :: TopLine,
                               cursor :: Location,
                               contents :: String } deriving (Eq,Show)

initialBuffer :: String -> EditBuffer
initialBuffer = EditBuffer 0 (0,0)

empty :: EditBuffer
empty = initialBuffer ""

lineCount :: EditBuffer -> Int
lineCount = (length . lines) . contents

insertChar :: Char -> EditBuffer -> EditBuffer
insertChar ch buffer
    | ch == '\n' = moveDown (buffer {contents = newContents})
    | otherwise  = moveRight (buffer {contents = newContents})
  where newContents      = before ++ [ch] ++ after
        (before, after)  = split buffer

insertString :: String -> EditBuffer -> EditBuffer
insertString str buffer =
    foldl (\b ch -> insertChar ch b) buffer str

deleteCharForward :: EditBuffer -> EditBuffer
deleteCharForward buffer
    | currentLineLength buffer == 0 = buffer
    | otherwise                      = resetCursor (buffer {contents = newContents})
  where newContents     = before ++ tail after
        (before, after) = split buffer

deleteCharBackward :: EditBuffer -> EditBuffer
deleteCharBackward buffer
    | currentLineLength buffer == 0 = buffer
    | otherwise                     = 
        case before of
            [] -> buffer
            _  -> moveLeft (buffer {contents = init before ++ after})
    where (before, after) = split buffer

replaceChar :: Char -> EditBuffer -> EditBuffer
replaceChar ch' buffer = 
    buffer {contents = before ++ [ch'] ++ tail after }
    where (before, after) = splitAt (absPosition buffer) (contents buffer)
 
insertLineAfter :: EditBuffer -> EditBuffer
insertLineAfter buffer =
    moveDown $ buffer {contents = unlines (b ++ [""] ++ a)}
    where (b,a) = splitAt ((snd.cursor) buffer) ((lines.contents) buffer)

deleteLine :: EditBuffer ->EditBuffer
deleteLine buffer = resetCursor (buffer {contents = newContents})
  where newContents = unlines [ line | (line, pos) <- numberedLines (contents buffer), pos /= snd (cursor buffer)] 

setCursorX :: Int -> EditBuffer -> EditBuffer
setCursorX nx buffer =
    buffer {cursor = (boundedValue bound nx, snd $ cursor buffer)}
    where bound = currentLineLength buffer

setCursorY :: Int -> EditBuffer -> EditBuffer
setCursorY ny buffer =
    buffer {cursor = (fst $ cursor buffer, boundedValue bound ny)}
    where bound = lineCount buffer

-- sets the cursor at a specific location, bounded by buffer constraints
setCursor :: Int -> Int -> EditBuffer -> EditBuffer
setCursor nx ny = setCursorX nx . setCursorY ny

-- if the bounds have changed (say a line or char was deleted) then
-- one should reset the cursor to adjust for edge conditions
resetCursor :: EditBuffer -> EditBuffer
resetCursor buf@(EditBuffer _ (x,y) _) =
    setCursor x y buf

-- updates the cursor by a particular offset given by (ax, ay)
-- example if the cursor were (0,0)
-- moveCursor 1 2 buf
-- would return a cursor at (1,2)
moveCursor :: Int -> Int -> EditBuffer -> EditBuffer
moveCursor ax ay buf@(EditBuffer _ (x,y) _) =
    setCursor (x + ax) (y + ay) buf

-- bounds a value between 0 and (bound - 1)
boundedValue :: Int -> Int -> Int
boundedValue bound value
  | bound <= 1      = 0
  | value <= 0      = 0
  | value >= bound  = bound - 1 
  | otherwise       = value

moveLeft, moveRight, moveUp, moveDown :: EditBuffer -> EditBuffer
moveLeft  = moveCursor (-1) 0  
moveRight = moveCursor 1 0
moveUp    = moveCursor 0 (-1)
moveDown  = moveCursor 0 1

moveToHome :: EditBuffer -> EditBuffer
moveToHome = setCursor 0 0

moveToEnd :: EditBuffer -> EditBuffer
moveToEnd = setCursor lastPos lastPos
  where lastPos = (maxBound :: Int) - 1

moveToLine :: Int -> EditBuffer -> EditBuffer
moveToLine = setCursor 0

moveToLineStart :: EditBuffer -> EditBuffer
moveToLineStart buf@(EditBuffer _ (_,y) _) = setCursor 0 y buf

moveToLineEnd :: EditBuffer -> EditBuffer
moveToLineEnd buf@(EditBuffer _ (_,y) _) = 
    setCursor (currentLineLength buf) y buf

wordForward :: EditBuffer -> EditBuffer
wordForward buffer =
  case dropSpaces . dropWord . drop (absPosition buffer) . numberedElements $ contents buffer of
    []            -> buffer
    ((_,pos) : _) -> buffer {cursor = (locationFromPosition pos (contents buffer))}

wordBackward :: EditBuffer -> EditBuffer
wordBackward buffer@(EditBuffer _ _ cont) = 
  case dropWord . dropSpaces . reverse . take (absPosition buffer) . numberedElements $ cont of
    []            -> buffer {cursor = locationFromPosition 0 cont}
    ((_,pos) : _) -> buffer {cursor = (locationFromPosition (pos+1) cont)}

-- returns the line at the cursor
currentLine :: EditBuffer -> String
currentLine (EditBuffer _ _ "") = ""
currentLine buffer
  | (y < 0) || (y >= lineCount buffer)  = ""
  | otherwise                             = lines (contents buffer) !! y
  where y = snd $ cursor buffer

-- returns the horizontal line length at the cursor
currentLineLength :: EditBuffer -> Int
currentLineLength = length . currentLine

split :: EditBuffer -> (String,String)
split buffer = splitAt (absPosition buffer) (contents buffer)

absPosition :: EditBuffer -> Int
absPosition (EditBuffer _ (x,y) cont) =
  (x+) . length . unlines . take y . lines $ cont

locationFromPosition :: Int -> String -> Location
locationFromPosition pos cont =
  let foreLines = init . lines . take (pos + 1) $ cont
      x         = pos - length (unlines foreLines) 
      y         = length foreLines
  in (x, y)

isPunct :: Char -> Bool
isPunct ch = isAscii ch && not (isAlphaNum ch) && not (isSpace ch) && not (isControl ch)  

dropWord :: [(Char,a)] -> [(Char,a)]
dropWord [] = []
dropWord allWords@((ch,_):_) 
  | isPunct ch    = dropPuncts allWords
  | isAlphaNum ch = dropAlphaNums allWords
  | otherwise     = allWords

dropPuncts, dropSpaces, dropAlphaNums :: [(Char,a)] -> [(Char,a)]
dropPuncts = dropInNumbered isPunct 
dropSpaces = dropInNumbered isSpace
dropAlphaNums = dropInNumbered isAlphaNum

dropInNumbered :: (Char -> Bool) -> [(Char,a)] -> [(Char,a)]
dropInNumbered f = dropWhile (\(ch,_) -> f ch)

numberedElements :: [a] -> [(a,Int)]
numberedElements = flip zip [0..]

numberedLines :: String -> [(String,Int)]
numberedLines = numberedElements . lines
