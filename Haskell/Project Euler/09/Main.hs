#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.

Find the product abc.

Answer: 31875000
-}

module Main where

import           Data.List (find)

main :: IO ()
main = print
  $ listSolution 500
--  $ searchSolution 500

searchSolution :: Int -> Int
searchSolution cap =
  case found of
    Just n  -> (\(a, b, c) -> a*b*c) n
    Nothing -> 0

  where
    sumsTo (a, b, c) n = a + b + c == n

    found = find (\t -> (t `sumsTo` 1000) && isTriplet t)
      $ allCombinations cap

listSolution :: Int -> Int
listSolution cap =
  (\(a, b, c) -> a*b*c)
  $ head
  $ filter isTriplet
  $ filter (\(a, b, c) -> a + b + c == 1000)
  $ allCombinations cap

allCombinations :: Int -> [(Int, Int, Int)]
allCombinations cap =
  [(a, b, c) | a <- [     1..cap]
             , b <- [succ a..cap]
             , c <- [succ b..cap]
             ]

isTriplet :: (Int, Int, Int) -> Bool
isTriplet (a, b, c) = a*a + b*b == c*c
