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

module Hed.EditBuffer
     ( EditBuffer(..)
     , Location
     , emptyBuffer
     , enterCommandMode
     , getBufferContents
     , lineCount
     , insertChar, deleteCharForward, deleteCharBackward, replaceChar, insertString
     , insertLineAfter
     , deleteLine
     , moveLeft, moveRight, moveUp, moveDown
     , moveToHome, moveToEnd, moveToLine
     , moveToLineStart, moveToLineEnd
     , wordForward, wordBackward
     , frame
     , showRepresentation
     , initialBuffer
     , splitBuffer
     , getCursor
     ) where

import Data.Char

type Location = (Int, Int)
type TopLine = Int
data EditBuffer = EditBuffer TopLine Location String deriving (Eq,Show)

initialBuffer s = EditBuffer 0 (0,0) s
emptyBuffer = initialBuffer ""

enterCommandMode :: EditBuffer -> EditBuffer
enterCommandMode = resetCursor

getBufferContents:: EditBuffer -> String
getBufferContents (EditBuffer _ _ contents) = contents

lineCount :: EditBuffer -> Int
lineCount (EditBuffer _ _ contents) = length . lines $ contents

insertChar :: Char -> EditBuffer -> EditBuffer
insertChar ch buffer@(EditBuffer topLine (x, y) contents)
    | ch == '\n' = EditBuffer topLine (0, y+1) newContents
    | otherwise  = EditBuffer topLine (x+1, y) newContents
  where newContents      = before ++ [ch] ++ after
        (before, after)  = split buffer

insertString :: String -> EditBuffer -> EditBuffer
insertString str buffer =
    foldl (\b ch -> insertChar ch b) buffer str

deleteCharForward :: EditBuffer -> EditBuffer
deleteCharForward buffer@(EditBuffer topLine location@(x,y) contents) 
    | (currentLineLength buffer == 0) = buffer
    | otherwise                       = resetCursor (EditBuffer topLine location newContents)
  where newContents     = before ++ (tail after)
        (before, after) = split buffer

deleteCharBackward :: EditBuffer -> EditBuffer
deleteCharBackward buffer@(EditBuffer topLine location@(x,y) contents) 
    | (currentLineLength buffer == 0) = buffer
    | otherwise                = 
        case before of
            [] -> buffer
            _  -> moveLeft (EditBuffer topLine location (init before ++ after))
    where (before, after) = split buffer

init' [] = []
init' ls = init ls

replaceChar :: Char -> EditBuffer -> EditBuffer
replaceChar replacementChar buffer@(EditBuffer topLine location contents) =
  let newContents  = map f . numberedElements $ contents 
      f (ch, pos)  = if pos == (absPosition buffer) then replacementChar else ch 
  in EditBuffer topLine location newContents
 
insertLineAfter :: EditBuffer -> EditBuffer
insertLineAfter (EditBuffer topLine _ "") = EditBuffer topLine (0,1) "\n"
insertLineAfter (EditBuffer topLine (_,y) contents) = EditBuffer topLine (0,y+1) newContents
  where newContents   = unlines . map f .  numberedLines $ contents
        f (line, pos) = if pos == y then line ++ "\n" else line  

deleteLine :: EditBuffer ->EditBuffer
deleteLine (EditBuffer topLine location@(_,y) contents) = resetCursor (EditBuffer topLine location newContents)
  where newContents = unlines [ line | (line, pos) <- numberedLines contents, pos /= y] 

setCursorX :: Int -> EditBuffer -> EditBuffer
setCursorX nx buf@(EditBuffer topLine (x,y) contents) =
    EditBuffer topLine (boundedValue bound nx, y) contents
    where bound = currentLineLength buf

setCursorY :: Int -> EditBuffer -> EditBuffer
setCursorY ny buf@(EditBuffer topLine (x,y) contents) =
    EditBuffer topLine (x, boundedValue bound ny) contents
    where bound = lineCount buf

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
moveToLine lineNumber = setCursor 0 lineNumber

moveToLineStart :: EditBuffer -> EditBuffer
moveToLineStart buf@(EditBuffer _ (_,y) _) = setCursor 0 y buf

moveToLineEnd :: EditBuffer -> EditBuffer
moveToLineEnd buf@(EditBuffer _ (_,y) _) = 
    setCursor (currentLineLength buf) y buf

wordForward :: EditBuffer -> EditBuffer
wordForward buffer@(EditBuffer topLine _ contents) = 
  case dropSpaces . dropWord . drop (absPosition buffer) . numberedElements $ contents of
    []            -> buffer
    ((_,pos) : _) -> EditBuffer topLine (locationFromPosition pos contents) contents

wordBackward :: EditBuffer -> EditBuffer
wordBackward buffer@(EditBuffer topLine _ contents) = 
  case dropWord . dropSpaces . reverse . take (absPosition buffer) . numberedElements $ contents of
    []            -> EditBuffer topLine (locationFromPosition 0 contents) contents
    ((_,pos) : _) -> EditBuffer topLine (locationFromPosition (pos+1) contents) contents

frame :: EditBuffer -> EditBuffer
frame buffer@(EditBuffer topLine (x,y) contents) 
  | y > topLine + 40 = EditBuffer (y - 40) (x,y) contents
  | y < topLine      = EditBuffer y (x,y) contents
  | otherwise        = buffer 

showRepresentation :: EditBuffer -> String
showRepresentation (EditBuffer topLine location contents) =
  show topLine ++ " " ++ show location ++ " " ++ show contents

-- returns the line at the cursor
currentLine :: EditBuffer -> String
currentLine (EditBuffer _ _ "") = ""
currentLine buffer@(EditBuffer _ (_, y) contents) 
  | (y < 0) || (y >= (lineCount buffer))  = ""
  | otherwise                             = (lines contents) !! y

-- returns the horizontal line length at the cursor
currentLineLength :: EditBuffer -> Int
currentLineLength = length . currentLine

split :: EditBuffer -> (String,String)
split buffer@(EditBuffer _ _ contents) = splitAt point contents
  where point = absPosition buffer 

splitBuffer buf@(EditBuffer _ _ []) =
    ("", ' ', "")

splitBuffer buf@(EditBuffer _ _ contents) =
    (left, head right, tail right)
    where (left, right) = split buf

getCursor buf = c
    where (_, c, _) = splitBuffer buf
  
absPosition :: EditBuffer -> Int
absPosition (EditBuffer _ (x, y) contents) = 
  (x+) . length . unlines . take y . lines $ contents

locationFromPosition :: Int -> String -> Location
locationFromPosition pos contents =
  let foreLines = init . lines . take (pos + 1) $ contents
      x         = pos - (length $ unlines foreLines) 
      y         = length foreLines
  in (x, y)

isPunct :: Char -> Bool
isPunct ch = isAscii ch && not (isAlphaNum ch) && not (isSpace ch) && not (isControl ch)  

dropWord :: [(Char,a)] -> [(Char,a)]
dropWord [] = []
dropWord all@((ch,_):_) 
  | isPunct ch    = dropPuncts all
  | isAlphaNum ch = dropAlphaNums all 
  | otherwise     = all

dropPuncts, dropSpaces, dropAlphaNums :: [(Char,a)] -> [(Char,a)]
dropPuncts = dropInNumbered isPunct 
dropSpaces = dropInNumbered isSpace
dropAlphaNums = dropInNumbered isAlphaNum

dropInNumbered :: (Char -> Bool) -> [(Char,a)] -> [(Char,a)]
dropInNumbered f = dropWhile (\(ch,_) -> f ch)

numberedElements :: [a] -> [(a,Int)]
numberedElements = (flip zip) [0..]

numberedLines :: String -> [(String,Int)]
numberedLines = numberedElements . lines
