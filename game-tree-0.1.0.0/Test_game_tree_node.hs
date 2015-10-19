-- FROM: http://code.haskell.org/game-tree/tests/Test_game_tree_node.hs
-- | Game tree nodes for algorithm testing
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

module Test_game_tree_node where

import Data.Tree.Game_tree.Negascout
import Data.Tree.Game_tree.Game_tree
import Test.HUnit

data Test_game_tree_node
    = Node 
      Int  -- Node number 
      Int  -- Node value
      deriving (Eq, Show)

instance Game_tree Test_game_tree_node
    where
      is_terminal (Node n v) = n > 18
      node_value  (Node n v) = v
      children  (Node n v) = fixed_children (Node n v)

main :: IO ()
main = do
  putStrLn "Testing negamax"
  counts_negamax <- runTestTT $ tests negamax
  putStrLn "Testing alpha_beta"
  counts_alpha_beta <- runTestTT $ tests alpha_beta_search
  putStrLn "Testing principal_variation_search"
  counts_pvs <- runTestTT $ tests principal_variation_search
  putStrLn "Testing negascout"
  counts_pvs <- runTestTT $ tests negascout
  return ()

-- | Fixed tree - see http://upload.wikimedia.org/wikipedia/commons/thumb/9/91/AB_pruning.svg/400px-AB_pruning.svg.png
--
-- 666 is used as for infinity
fixed_children (Node n v) =
    case n of
      0 ->  [(test_node 1), (test_node 2), (test_node 3)]
      1 ->  [(test_node 4), (test_node 5)]
      2 ->  [(test_node 6), (test_node 7)]
      3 ->  [(test_node 8), (test_node 9)]
      4 ->  [(test_node 10), (test_node 11)]
      5 ->  [(test_node 12)]
      6 ->  [(test_node 13), (test_node 14)]
      7 ->  [(test_node 15)]
      8 ->  [(test_node 16)]
      9 ->  [(test_node 17), (test_node 18)]
      10 -> [(test_node 19), (test_node 20)]
      11 -> [(test_node 21), (test_node 22), (test_node 23)]
      12 -> [(test_node 24)]
      13 -> [(test_node 25)]
      14 -> [(test_node 26), (test_node 27)]
      15 -> [(test_node 28)]
      16 -> [(test_node 29)]
      17 -> [(test_node 30), (test_node 31)]
      18 -> [(test_node 32)]
      _ -> []

test_node :: Int -> Test_game_tree_node
test_node n = Node n (snd (test_value n negamax))


root_node :: Int -> Test_game_tree_node
root_node n = 
    let v = case n of
              n' | n' < 19 -> 0 -- dummy value
                 | n' == 19 -> 5
                 | n' == 20 -> 6
                 | n' == 21 -> 7
                 | n' == 22 -> 4
                 | n' == 23 -> 5
                 | n' == 24 -> 3
                 | n' == 25 -> 6
                 | n' == 26 -> 6
                 | n' == 27 -> 9
                 | n' == 28 -> 7
                 | n' == 29 -> 5
                 | n' == 30 -> 9
                 | n' == 31 -> 8
                 | n' == 32 -> 6
    in Node n v

test_value :: Int -> (Test_game_tree_node -> Int -> ([Test_game_tree_node], Int)) -> ([Test_game_tree_node], Int)
test_value n f = f (Node n v) 10
    where v = case n of
                n' | n' < 19 -> 0 -- dummy value
                   | n' == 19 -> 5
                   | n' == 20 -> 6
                   | n' == 21 -> 7
                   | n' == 22 -> 4
                   | n' == 23 -> 5
                   | n' == 24 -> 3
                   | n' == 25 -> 6
                   | n' == 26 -> 6
                   | n' == 27 -> 9
                   | n' == 28 -> 7
                   | n' == 29 -> 5
                   | n' == 30 -> 9
                   | n' == 31 -> 8
                   | n' == 32 -> 6

-- | Test cases.
-- N.B. Negative values result from the odd-numbered depths (root = 0), as the
-- evaluator function is not sensitive to whose turn it is (it should be for negascout).
tests f = TestList [TestCase (assertEqual "full tree" ([(root_node 0),(test_node 2),(test_node 6),(test_node 13),(test_node 25)], 6) (test_value 0 f)),
         TestCase (assertEqual "Node 1" ([(root_node 1), (test_node 5), (test_node 12), (test_node 24)], -3) (test_value 1 f)),
         TestCase (assertEqual "Node 2" ([(root_node 2), (test_node 6), (test_node 13), (test_node 25)], -6) (test_value 2 f)),
         TestCase (assertEqual "Node 3" ([(root_node 3), (test_node 8), (test_node 16), (test_node 29)], -5) (test_value 3 f)),
         TestCase (assertEqual "Node 4" ([(root_node 4), (test_node 10), (test_node 19)], 5) (test_value 4 f)),
         TestCase (assertEqual "Node 5" ([(root_node 5), (test_node 12), (test_node 24)], 3) (test_value 5 f)),
         TestCase (assertEqual "Node 6" ([(root_node 6), (test_node 13), (test_node 25)], 6) (test_value 6 f)),
         TestCase (assertEqual "Node 7" ([(root_node 7), (test_node 15), (test_node 28)], 7) (test_value 7 f)),
         TestCase (assertEqual "Node 8" ([(root_node 8), (test_node 16), (test_node 29)], 5) (test_value 8 f)),
         TestCase (assertEqual "Node 9" ([(root_node 9), (test_node 17), (test_node 31)], 8) (test_value 9 f)),
         TestCase (assertEqual "Node 10" ([(root_node 10), (test_node 19)], -5) (test_value 10 f)),
         TestCase (assertEqual "Node 11" ([(root_node 11), (test_node 22)], -4) (test_value 11 f)),
         TestCase (assertEqual "Node 12" ([(root_node 12), (test_node 24)], -3) (test_value 12 f)),
         TestCase (assertEqual "Node 13" ([(root_node 13), (test_node 25)], -6) (test_value 13 f)),
         TestCase (assertEqual "Node 14" ([(root_node 14), (test_node 26)], -6) (test_value 14 f)),
         TestCase (assertEqual "Node 15" ([(root_node 15), (test_node 28)], -7) (test_value 15 f)),
         TestCase (assertEqual "Node 16" ([(root_node 16), (test_node 29)], -5) (test_value 16 f)),
         TestCase (assertEqual "Node 17" ([(root_node 17), (test_node 31)], -8) (test_value 17 f)),
         TestCase (assertEqual "Node 18" ([(root_node 18), (test_node 32)], -6) (test_value 18 f)),
         TestCase (assertEqual "Node 19" ([(root_node 19)], 5) (test_value 19 f)),
         TestCase (assertEqual "Node 20" ([(root_node 20)], 6) (test_value 20 f)),
         TestCase (assertEqual "Node 21" ([(root_node 21)], 7) (test_value 21 f)),
         TestCase (assertEqual "Node 22" ([(root_node 22)], 4) (test_value 22 f)),
         TestCase (assertEqual "Node 23" ([(root_node 23)], 5) (test_value 23 f)),
         TestCase (assertEqual "Node 24" ([(root_node 24)], 3) (test_value 24 f)),
         TestCase (assertEqual "Node 25" ([(root_node 25)], 6) (test_value 25 f)),
         TestCase (assertEqual "Node 26" ([(root_node 26)], 6) (test_value 26 f)),
         TestCase (assertEqual "Node 27" ([(root_node 27)], 9) (test_value 27 f)),
         TestCase (assertEqual "Node 28" ([(root_node 28)], 7) (test_value 28 f)),
         TestCase (assertEqual "Node 29" ([(root_node 29)], 5) (test_value 29 f)),
         TestCase (assertEqual "Node 30" ([(root_node 30)], 9) (test_value 30 f)),
         TestCase (assertEqual "Node 31" ([(root_node 31)], 8) (test_value 31 f)),
         TestCase (assertEqual "Node 32" ([(root_node 32)], 6) (test_value 32 f))
        ]
