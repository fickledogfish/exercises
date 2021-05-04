#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                              Maximum path sum II

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

    {3}
    {7} 4
     2 {4} 6
     8  5 {9} 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt (right click and 'Save
Link/Target As...'), a 15K text file containing a triangle with one-hundred
rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible to
try every route to solve this problem, as there are 2^99 altogether! If you
could check one trillion (10^12) routes every second it would take over twenty
billion years to check them all. There is an efficient algorithm to solve
it. ;o)

Answer: 7273
-}

module Main where

import           Control.Exception (Exception, throw)
import           Data.List.Index   (indexed)

main :: IO ()
main = do
  { --triangle <- readFile "sample_triangle.txt"
    triangle <- readFile "triangle.txt"
  ; let
      (_, s) = bottomUpSolution $ readTriangle triangle
    in
      putStrLn $ show s
  }

bottomUpSolution :: Triangle -> ([Counter], TriangleEl)
bottomUpSolution t =
  aux [] (-1, t)

  where
    aux :: [Counter] -> (Counter, Triangle) -> ([Counter], TriangleEl)
    aux idxAcc (i, t) =
      let
        lenIdxAcc = length idxAcc -- length of the previous index accumulator
        newIdxAcc = i:idxAcc      -- the new accumulator list
      in
        if rows t < 2 then
          -- The last element of the index accumulator is invalid, so drop it.
          (take lenIdxAcc newIdxAcc, head $ values t)
        else
          aux (newIdxAcc) $ newTriangle t

    newTriangle :: Triangle -> (Counter, Triangle)
    newTriangle t =
      let
        prevRows     = rows t
        lastRowNum   = pred prevRows
        sndLstRowNum = pred lastRowNum
      in let
        lastRow   = t `row` lastRowNum
        sndLstRow = t `row` sndLstRowNum
      in let
        prevRows     = take (numElements sndLstRowNum) $ values t
        combinedRows = combine (indexed sndLstRow) (indexed lastRow) []
      in let
        (idxMaxSum, _) = foldr maxEl (0, 0) combinedRows
        newLastRow     = map (\(_, e) -> e) combinedRows
      in
        ( idxMaxSum
        , Triangle { rows = lastRowNum
                   , values = prevRows ++ newLastRow
                   }
        )

    maxEl :: (Counter, TriangleEl) -> (Counter, TriangleEl)
          -> (Counter, TriangleEl)
    maxEl el@(_, e) rcrd@(_, re) = if e > re then el else rcrd

    combine :: (Num b, Ord b, Show a, Show b) => [(a, b)] -> [(a, b)]
            -> [(a, b)] -> [(a, b)]
    combine [] _ acc               = reverse  acc
    combine (h:t) l2@(e1:e2:_) acc = combine t (tail l2) $ (maxSum h e1 e2):acc

    maxSum :: (Num b, Ord b) => (a, b) -> (a, b) -> (a, b) -> (a, b)
    maxSum (_, e1) (i2, e2) (i3, e3) =
      let s2 = e1 + e2
          s3 = e1 + e3
      in if s2 > s3 then
           (i2, s2)
         else
           (i3, s3)

row :: Triangle -> Counter -> [TriangleEl]
row t r = map (t `at`) [(r, c) | c <- [0..r]]

at :: Triangle -> Index -> TriangleEl
at t idx = values t !! (index idx)

index :: Index -> Counter
index (row, col) =
  if col > row then
    throw $ IndexError(row, col)
  else
    row * (row + 1) `div` 2 + col

data TriangleError = IndexError(Counter, Counter)

instance Show TriangleError where
  show e =
    case e of
      IndexError(row, col) ->
        "Index out of bounds: (" ++ (show row) ++ ", " ++ (show col) ++ ")"

instance Exception TriangleError

type Index = (Counter, Counter)

readTriangle :: String -> Triangle
readTriangle s =
  Triangle { rows   = length lns
           , values = map read $ concat lns
           }

  where
    lns :: [[String]]
    lns = map words $ lines s

-- Calculate the number of elements a Triangle of n rows would have.
numElements :: Counter -> Counter
numElements rows = sum $ [1..rows]

data Triangle = Triangle
  { rows   :: Counter
  , values :: [TriangleEl]
  } deriving(Show)

type TriangleEl = Int
type Counter    = Int
