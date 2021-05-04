#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                               15: Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

    #######    ####--+    ####--+
    |  |  #    |  #  |    |  #  |
    +--+--#    +--####    +--#--+
    |  |  #    |  |  #    |  #  |
    +--+--#    +--+--#    +--####

    #--+--+    #--+--+    #--+--+
    #  |  |    #  |  |    #  |  |
    #######    ####--+    #--+--+
    |  |  #    |  #  |    #  |  |
    +--+--#    +--####    #######

How many such routes are there through a 20×20 grid?

Answer: 137846528820
-}

module Main where

main :: IO ()
main = print
  $ numLatticePaths
  $ newSquareGrid 20

binomCoeff :: (Int, Int) -> Int
binomCoeff (n, 0) = 1
binomCoeff (0, k) = 0
binomCoeff (n, k) = binomCoeff (pred n, pred k) * n `div` k

numLatticePaths :: Grid -> Int
numLatticePaths Grid { rows = n, cols = k } =
  binomCoeff (n + k, k)

newSquareGrid :: Int -> Grid
newSquareGrid size = newGrid size size

newGrid :: Int -> Int -> Grid
newGrid rows cols = Grid
  { rows = rows
  , cols = cols
  }

data Grid = Grid
  { rows :: Int
  , cols :: Int
  } deriving(Show)
