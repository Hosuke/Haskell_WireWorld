-- Original:
-- Uwe R. Zimmer
-- Australia 2012
--
-- Modified by:
-- Huang Geyang
-- 16 March 2014
--

module Transitions.For_List_2D (
    transition_world -- :: List_2D Cell -> List_2D Cell
) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.List_2D


transition_world :: List_2D Cell -> List_2D Cell
transition_world world = transition world world  --copy another world
    where   transition :: List_2D Cell -> List_2D Cell -> List_2D Cell
            transition world world_copy = case world of
                (Conductor, (x, y)) : xs -> case element_occurrence Head (local_elements (x, y) world_copy) of
                    1 -> (Head, (x, y)) : transition xs world_copy
                    2 -> (Head, (x, y)) : transition xs world_copy
                    _ -> (Conductor, (x, y)) : transition xs world_copy
                (Head, (x, y)) : xs -> (Tail, (x, y)) : transition xs world_copy
                (Tail, (x, y)) : xs -> (Conductor, (x, y)) : transition xs world_copy
                (Empty, (x, y)) : xs -> (Empty, (x, y)) : transition xs world_copy
                [] -> []
