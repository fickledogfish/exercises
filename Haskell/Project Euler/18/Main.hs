#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                             18: Maximum path sum I

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

    {3}
    {7} 4
    2  {4} 6
    8   5 {9} 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

    75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by
trying every route. However, Problem 67, is the same challenge with a triangle
containing one-hundred rows; it cannot be solved by brute force, and requires a
clever method! ;o)

Answer: 1074
-}

module Main where

import           Control.Exception (Exception, throw)
import           Data.List.Index   (indexed)

main :: IO ()
main =
  putStrLn
  $ show

  -- First attempt at the solution: always go towards the biggest number. This
  -- leads to the answer 1064, which is wrong.
  --
  --     $ followBiggestNumberSolution triangle

  -- Second attempt: work from the bottom up, select the biggest sum for each
  -- element and replace the previous row with it.
  $ bottomUpSolution triangle

bottomUpSolution :: Triangle -> ([Int], Int)
bottomUpSolution t =
  aux [] (-1, t)

  where
    aux :: [Int] -> (Int, Triangle) -> ([Int], Int)
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

    newTriangle :: Triangle -> (Int, Triangle)
    newTriangle t =
      let
        prevRows       = rows t
        lastRowNum     = pred prevRows
        sndLstRowNum   = pred lastRowNum
      in let
        lastRow        = t `row` lastRowNum
        sndLstRow      = t `row` sndLstRowNum
      in let
        prevRows       = take (numElements sndLstRowNum) $ values t
        combinedRows   = combine (indexed sndLstRow) (indexed lastRow) []
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

followBiggestNumberSolution :: Triangle -> Int
followBiggestNumberSolution t =
  sum
  $ map (t `at`)
  $ followBiggestNumber t (0, 0) [(0, 0)]

followBiggestNumber :: Triangle -> Index -> [Index] -> [Index]
followBiggestNumber t idx acc =
  -- won't work because this'll attempt to evaluate `at (14, 7)`, which will
  -- lead to a move outside the bounds of the provided triangle.
  --
  --     takeWhile (\(r, _) -> r <= rows)
  --     $ iterate nextMove (0, 0)

  if (succ nextRow) < rows t then
    followBiggestNumber t next $ next : acc
  else
    next : acc

  where
    next@(nextRow, nextCol) = nextMove idx

    nextMove :: Index -> Index
    nextMove =
      maxAt t . possibleMoves

maxAt :: Triangle -> [Index] -> Index
maxAt t idxs =
  aux idxs ((0, 0), 0)

  where
    aux :: [Index] -> (Index, Int) -> Index
    aux [] (acci, _) = acci
    aux (i:is) (acci, accv) =
      aux is $ if (t `at` i) > accv then
                 (i, (t `at` i))
               else
                 (acci, accv)

possibleMoves :: Index -> [Index]
possibleMoves (row, col) =
  take 2
  $ iterate
  (\(r, c) -> (r, succ c))
  (succ row, col)

row :: Triangle -> Int -> [Int]
row t r = map (t `at`) [(r, c) | c <- [0..r]]

at :: Triangle -> Index -> Int
at t idx = values t !! (index idx)

index :: Index -> Int
index (row, col) =
  if col > row then
    throw $ IndexError(row, col)
  else
    row * (row + 1) `div` 2 + col

data TriangleError = IndexError(Int, Int)

instance Show TriangleError where
  show e =
    case e of
      IndexError(row, col) ->
        "Index out of bounds: (" ++ (show row) ++ ", " ++ (show col) ++ ")"

instance Exception TriangleError

type Index = (Int, Int)

triangle :: Triangle
triangle = Triangle
  { rows   = 15
  , values = [ 75
             , 95, 64
             , 17, 47, 82
             , 18, 35, 87, 10
             , 20, 04, 82, 47, 65
             , 19, 01, 23, 75, 03, 34
             , 88, 02, 77, 73, 07, 63, 67
             , 99, 65, 04, 28, 06, 16, 70, 92
             , 41, 41, 26, 56, 83, 40, 80, 70, 33
             , 41, 48, 72, 33, 47, 32, 37, 16, 94, 29
             , 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14
             , 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57
             , 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48
             , 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31
             , 04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23
             ]
  }

sampleTriangle :: Triangle
sampleTriangle = Triangle
  { rows   = 4
  , values = [ 3
             , 7, 4
             , 2, 4, 6
             , 8, 5, 9, 3
             ]
  }

-- Calculate the number of elements a Triangle of n rows would have.
numElements :: Int -> Int
numElements rows = sum $ [1..rows]

data Triangle = Triangle
  { rows   :: Int
  , values :: [Int]
  } deriving(Show)
