-- | Nodes in game trees
-- Copyright 2009 Colin Adams
--
-- This file is part of game-tree.
--
--  Game-tree is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  Game-tree is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with game-tree.  If not, see <http://www.gnu.org/licenses/>.

module Data.Tree.Game_tree.Game_tree where

-- | Nodes in a game search tree
class Game_tree a where
    -- | Is this a game-terminating node (e.g. checkmate)?
    --
    -- Law: is_terminal n == (children n == [])
    is_terminal ::  a -> Bool
    -- | Heuristic value of node
    -- 
    -- Returned value must line in the (inclusive) range (minBound + 4, maxBound - 3)
    -- Needs to be sensitive to whose turn it is to move.
    -- I.e. it must return values of the opposite sign if the other player is to move.
    node_value :: a -> Int
    -- | Child nodes in the game tree (positions more deeply searched)
    children :: a -> [a]


