module Hed.VTY where

import Graphics.Vty
import System.IO
import Data.Word

import Hed.Buffer

data Window = Window { file :: Maybe String
                     , buffer :: EditBuffer
                     }

start :: Window -> IO ()
start win = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    play vt w h win

pieceA, dumpA, statusA :: Attr
pieceA = def_attr `with_fore_color` red
dumpA = def_attr `with_fore_color` white
statusA = def_attr `with_back_color` green

play :: Vty -> Word -> Word -> Window -> IO () 
play vt w h win@(Window file' buf) = do
     update vt $ render (fromEnum w) (fromEnum h) win
     k <- next_event vt
     case k of 
        EvKey KLeft  []               -> play vt w h win{buffer = (moveLeft buf)}
        EvKey KRight []               -> play vt w h win{buffer = (moveRight buf)}
        EvKey KUp    []               -> play vt w h win{buffer = (moveUp buf)}
        EvKey KDown  []               -> play vt w h win{buffer = (moveDown buf)}
        EvKey KDel   []               -> play vt w h win{buffer = (deleteCharForward buf)}
        EvKey KEsc   []               -> shutdown vt >> return ()
        EvKey (KASCII 's') [MCtrl]    ->
            case file' of
                Just f ->
                    writeFile f(contents buf) >> play vt w h win
                Nothing ->
                    play vt w h $ Window Nothing (mkBuffer 0 0 "no file given, hit any key to continue")

        EvKey (KASCII 'r') [MCtrl]    -> refresh vt >> play vt w h win
        EvKey (KASCII 'h') [MCtrl]    -> play vt w h win{buffer = (deleteCharBackward buf)}
        EvKey (KASCII 'a') [MCtrl]    -> play vt w h win{buffer = (moveToLineStart buf)}
        EvKey (KASCII 'e') [MCtrl]    -> play vt w h win{buffer = (moveToLineEnd buf)}
        EvKey (KASCII 'w') [MCtrl]    -> play vt w h win{buffer = (wordForward buf)}
        EvKey (KASCII 'b') [MCtrl]    -> play vt w h win{buffer = (wordBackward buf)}
        EvKey (KASCII 'o') [MCtrl]    -> play vt w h win{buffer = (insertLineAfter buf)}
        EvKey (KASCII 'k') [MCtrl]    -> play vt w h win{buffer = (deleteLine buf)}
        EvKey (KASCII '\t') []        -> play vt w h win{buffer = (insertString buf "    ")}
        EvKey (KASCII ch) []          -> play vt w h win{buffer = (insertChar buf ch)}
        EvKey KEnter []               -> play vt w h win{buffer = (insertChar buf '\n')}
        EvResize nx ny                -> play vt (toEnum nx) (toEnum ny) win
        _                             -> play vt w h win

filename :: Maybe String -> String
filename file' = case file' of
    Just f -> f
    Nothing -> ""

renderStatus :: Int -> Int -> Window -> Image
renderStatus w _h (Window file' buf) = 
    string def_attr (take w (statusLine ++ repeat ' '))
    where (x,y) = cursor buf
          position = show x ++ "," ++ show y
          f = filename file'
          statusLine = f ++ take (w - length position - length f) (repeat ' ') ++  position
          

renderLine :: Int -> String -> Attr -> Image
renderLine width line attr =
    iso_10464_string attr $ take width (line ++ repeat ' ')

renderBuffer :: Int -> Int -> EditBuffer -> Image
renderBuffer w h buf = 
    vert_cat (map (\l -> renderLine w l dumpA) $ take h (lines (contents buf ++ repeat '\n')))

render :: Int -> Int -> Window -> Picture
render w h win@(Window _f buf) = (pic_for_image img){pic_cursor = Cursor (toEnum x) (toEnum y)}
    where (x,y) = cursor buf
          img = renderBuffer w (h - 1) buf <-> renderStatus w h win
