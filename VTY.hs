module VTY where
import Graphics.Vty
import System.IO
import qualified Data.ByteString.Char8 as B
import EditBuffer
import Data.Word

window s = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    play vt w h (initialBuffer s)

pieceA = def_attr `with_fore_color` red
dumpA = def_attr `with_fore_color` white
statusA = def_attr `with_back_color` green

play :: Vty -> Word -> Word -> EditBuffer -> IO () 
play vt w h buf = do
     update vt $ render buf (fromEnum w) (fromEnum h)
     k <- next_event vt
     case k of 
        EvKey (KASCII 'r') [MCtrl]    -> refresh vt >> play vt w h buf
        EvKey KLeft  []               -> play vt w h (moveLeft buf)
        EvKey KRight []               -> play vt w h (moveRight buf)
        EvKey KUp    []               -> play vt w h (moveUp buf)
        EvKey KDown  []               -> play vt w h (moveDown buf)
        EvKey KEsc   []               -> shutdown vt >> return ()
        EvKey KDel   []               -> play vt w h (deleteCharForward buf)
        EvKey (KASCII 'h') [MCtrl]    -> play vt w h (deleteCharBackward buf)
        EvKey (KASCII '\t') []        -> play vt w h (insertString "    " buf)
        EvKey (KASCII ch) []          -> play vt w h (insertChar ch buf)
        EvKey KEnter []               -> play vt w h (insertChar '\n' buf)
        EvResize nx ny                -> play vt (toEnum nx) (toEnum ny) buf
        _                             -> play vt w h buf

renderStatus buf@(EditBuffer _ (x,y) _) w h =
    string def_attr (take w (statusLine ++ repeat ' '))
    where statusLine = "Status -- Size: (" ++ show w ++ ", " ++ show h ++ ") " ++
                                 "Pos: (" ++ show x ++ ", " ++ show y ++ ")"

renderLine :: String -> Int -> Attr -> Image
renderLine line width attr =
    iso_10464_string attr $ (take width (line ++ repeat ' '))

renderBuffer :: EditBuffer -> Int -> Int -> Image
renderBuffer buf@(EditBuffer topLine (x,y) contents) w h =
    vert_cat (map (\l -> renderLine l w dumpA) $ take h (lines (contents ++ repeat '\n')))

render :: EditBuffer -> Int -> Int -> Picture
render buf@(EditBuffer _ (x,y) contents) w h = (pic_for_image i) {pic_cursor = Cursor (toEnum x) (toEnum y)}
    where i =  renderBuffer buf w (h - 1) <-> renderStatus buf w h 
