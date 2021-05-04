#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
2520 is the smallest number that can be divided by each of the numbers from 1 to
10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?

Answer: 232792560
-}

module Main where

import           Data.List (find, sortBy)

main :: IO ()
main = print
  $ stupidSolution 20

stupidSolution ::  Int -> Int
stupidSolution cap =
  prodFactors
  $ foldr (joinFactors . accumulateFactors . stupidFactor) [] [2..cap]

-- yeah, I know...
stupidFactor :: Int -> [Int]
stupidFactor 20 = [2, 2, 5]
stupidFactor 19 = [19]
stupidFactor 18 = [2, 3, 3]
stupidFactor 17 = [17]
stupidFactor 16 = [2, 2, 2, 2]
stupidFactor 15 = [3, 5]
stupidFactor 14 = [2, 7]
stupidFactor 13 = [13]
stupidFactor 12 = [2, 2, 3]
stupidFactor 11 = [11]
stupidFactor 10 = [2, 5]
stupidFactor 9  = [3, 3]
stupidFactor 8  = [2, 2, 2]
stupidFactor 7  = [7]
stupidFactor 6  = [2, 3]
stupidFactor 5  = [5]
stupidFactor 4  = [2, 2]
stupidFactor 3  = [3]
stupidFactor 2  = [2]

prodFactors :: [(Int, Int)] -> Int
prodFactors fs =
  product $ map (\(c, n) -> product $ replicate c n) fs

joinFactors :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
joinFactors =
  nmap joinFactor

  where
    nmap f a []     = a
    nmap f a (b:bs) = nmap f (f a b) bs

    sumFactor :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sumFactor (n1, f) (n2, _) = (max n1 n2, f)

    joinFactor :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    joinFactor fs factor =
      case findFactor factor fs of
        Just el -> sumFactor el factor : filter (neqFactor factor) fs
        Nothing -> factor:fs

neqFactor :: (Int, Int) -> (Int, Int) -> Bool
neqFactor f1 f2 = not $ eqFactor f1 f2

eqFactor :: (Int, Int) -> (Int, Int) -> Bool
eqFactor (_, n1) (_, n2) = n1 == n2

findFactor :: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
findFactor (_, n) = find (\(_, x) -> x == n)

sortFactors :: [(Int, Int)] -> [(Int, Int)]
sortFactors = sortBy (\(_, n1) (_, n2) -> compare n1 n2)

accumulateFactors :: [Int] -> [(Int, Int)]
accumulateFactors fs = gatherSimilar fs []

gatherSimilar :: [Int] -> [(Int, Int)] -> [(Int, Int)]
gatherSimilar [] acc = acc
gatherSimilar (x:xs) acc =
  gatherSimilar xs $ count $ find (\(_, n) -> x == n) acc

  where
    inc :: (Int, Int) -> (Int, Int)
    inc (occurencies, id) = (succ occurencies, id)

    count :: Maybe (Int, Int) -> [(Int, Int)]
    count Nothing   = (1, x) : acc
    count (Just el) = inc el : filter (/= el) acc

naiveSolution :: Int -> Int
naiveSolution cap =
  naiveSolution' 1

  where
    naiveSolution' :: Int -> Int
    naiveSolution' n =
      if n `isDivisibleByAllNumbersUnder` cap then
        n
      else
        naiveSolution' $ succ n

isDivisibleByAllNumbersUnder :: Int -> Int -> Bool
isDivisibleByAllNumbersUnder a n =
  all (== 0) [a `rem` x | x <- [2..n]]
