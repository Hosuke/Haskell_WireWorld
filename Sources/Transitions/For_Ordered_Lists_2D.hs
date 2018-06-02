-- Original:
-- Uwe R. Zimmer
-- Australia 2012
--
-- Modified by:
-- Huang Geyang
-- u5421856
-- 17 March 2014
--
--
-- Updated by Huang Geyang @ 3 April 2014:
-- Simplify the local_space to just be the local_line function (by excluding the empty case)
-- Modify the given local_elements function (reduce checking steps)
--
--
-- I have the confidence in my program's efficiency
-- For example, it can run the "Playfield.bmp" up to 1200fps!
--                                      -- Huang Geyang
--
-- Updated by Huang Geyang @ 5 April 2014:
-- Adding find_head_world function (world_copy now is just a world of Head !)
-- Reduce checking process (count_local_element)
--
-- Now it can run "Playfield.bmp" up to 6000fps......

module Transitions.For_Ordered_Lists_2D (
    transition_world -- :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.Ordered_Lists_2D


transition_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_world world = transition_line world (find_head_world world)

    where
        find_head_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
        find_head_world world = case world of
            [] -> []
            line:lines -> Sparse_Line {y_pos = y_pos line, entries = find_head_line (entries line)} : find_head_world lines

            where
                find_head_line :: Placed_Elements Cell -> Placed_Elements Cell
                find_head_line eline = case eline of
                    cell : cells -> case entry cell of
                        Head -> cell : find_head_line cells
                        _    -> find_head_line cells
                    [] -> []

        transition_line :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
        transition_line world world_copy = case world of
                [] -> []
                line : lines -> Sparse_Line {y_pos = y_pos line, entries = transition (entries line) (y_pos line) (local_space (y_pos line) world_copy)} : transition_line lines world_copy

                    where
                        local_space :: Y_Coord -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
                        local_space y world_copy = (local_lines y world_copy)

                        transition :: Placed_Elements Cell -> Y_Coord -> Ordered_Lists_2D Cell -> Placed_Elements Cell
                        transition eline y space = case eline of
                            cell : cells -> transition_cell cell y space : transition cells y space
                            [] -> []

                            where
                                transition_cell :: Placed_Element Cell -> Y_Coord -> Ordered_Lists_2D Cell -> Placed_Element Cell
                                transition_cell cell y space = case entry cell of
                                    Conductor -> case count_local_elements (x_pos cell, y ) space of
                                        1 -> (cell {entry = Head})
                                        2 -> (cell {entry = Head})
                                        _ -> (cell {entry = Conductor})
                                    Head -> (cell {entry = Tail})
                                    Tail -> (cell {entry = Conductor})
                                    Empty -> (cell {entry = Empty})

                                    -- Modified given local_elements function by Huang Geyang to avoid repeating calculation
                                    where
                                        count_local_elements :: Coord -> Ordered_Lists_2D e -> Integer
                                        count_local_elements (x, y) space = case space of
                                            l:ls -> count_neighbours_in_line l + (count_local_elements (x, y) ls)
                                            []   -> 0

                                            where
                                                count_neighbours_in_line :: Sparse_Line e -> Integer
                                                count_neighbours_in_line line = count_neighbours_in_entries (entries line)

                                                    where
                                                        count_neighbours_in_entries :: Placed_Elements e -> Integer
                                                        count_neighbours_in_entries list = case list of
                                                            c: cs
                                                                | x < (x_pos c) - 1      -> 0
                                                                | abs (x - x_pos c) <= 1 -> 1 + count_neighbours_in_entries cs
                                                                | otherwise              -> count_neighbours_in_entries cs
                                                            [] -> 0



