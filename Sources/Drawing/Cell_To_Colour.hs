--
-- Uwe R. Zimmer
-- Australia 2012
--

module Drawing.Cell_To_Colour (

    cell_to_colour -- :: Cell -> Color

) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Graphics.Gloss (Color, red, blue, yellow, green, black, dark, light, white)

cell_to_colour :: Cell -> Color
cell_to_colour cell = case cell of
    Head      -> white
    Tail      -> dark green
    Conductor -> black
    Empty     -> black
