-- Author: Matthew Meyer (matt.meyer134@gmail.com
module Piece --(exportFunctions) 
  where

import System.Random
import System.IO

data Shape = Line | Square | T | Z | ZPrime | L | LPrime deriving (Show)
-- Line      Square    T     Z       ZPrime   L      LPrime
-- [][][][]  [][]    [][][]  [][]      [][]      []  []
--           [][]      []      [][]  [][]    [][][]  [][][]

type Orientation = [(Int,Int)]

-- Shapes will be on 20x10 grid and fall
-- As they fall, their orientation will be kept
-- the same unless the user turns them
-- The pivot point is exactly like the original gameboy game (tested!)
--
-- Ex) Line can only have two states: vertical and horizontal
--     Pivot point in second from bottom when vertical and
--     second from left when horizontal
--     .       <- Line changing orientation           
--     .          o = pivot point
--   . o . .      . = moving points
--     .
--
-- All pivot points are (0,0) 
orientations :: Shape -> [Orientation]
orientations Line   = [[(-1,0), (0,0),  (1,0),  (2,0)],
                       [(0,-1), (0,0),  (0,1),  (0,2)]]
orientations Square = [[(0,0),  (0,1),  (1,0),  (1,1)]] -- doesnt rotate
orientations T      = [[(0,0),  (-1,0), (1,0),  (0,1)],
                       [(0,0),  (0,1),  (1,0),  (0,-1)],
                       [(0,0),  (1,0),  (0,-1), (-1,0)],
                       [(0,0),  (0,1),  (-1,0), (0,-1)]]
orientations Z      = [[(0,0),  (0,-1), (-1,0), (1,-1)],
                       [(0,0),  (0,1),  (-1,0), (-1,-1)]]
orientations ZPrime = [[(0,0),  (1,0),  (0,-1), (-1,-1)],
                       [(0,0),  (0,-1), (-1,0), (-1,1)]]
orientations L      = [[(0,0),  (0,1),  (0,-1), (1,-1)],
                       [(0,0),  (1,0),  (-1,0), (-1,-1)],
                       [(0,0),  (0,-1), (0,1),  (-1,1)],
                       [(0,0),  (-1,0), (1,0),  (1,1)]]
orientations LPrime = [[(0,0),  (0,1),  (0,-1), (-1,-1)],
                       [(0,0),  (-1,0), (1,0),  (1,-1)],
                       [(0,0),  (0,-1), (0,1),  (1,1)],
                       [(0,0),  (-1,0), (1,0),  (-1,1)]]

type Block = (Shape, Orientation)

turn :: Block -> Block
turn (s,o) = (s,orientations s !! or)
  where
    or = (1 + (at o $ orientations s)) `mod` (length $ orientations s)
    at elem lst
        | elem == lst !! 0 = 0
        | elem == lst !! 1 = 1
        | elem == lst !! 2 = 2
        | elem == lst !! 3 = 3

get :: (Num a, Eq a) => a -> Shape
get x 
    | x == 0 = Line
    | x == 1 = Square
    | x == 2 = T
    | x == 3 = Z
    | x == 4 = ZPrime
    | x == 5 = L
    | x == 6 = LPrime


make :: Shape -> Block
make s = (s, orientations s !! 0)

