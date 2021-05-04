#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

Answer: 1366
-}

module Main where

main :: IO ()
main = print
  $ sum
  $ toDigits
  $ 2^1000

toDigits :: (Integral a, Ord a) => a -> [a]
toDigits num =
  map fst
  $ takeWhile (\(d, n) -> d > 0 || n > 0)
  $ drop 1
  $ iterate (\(digit, num) -> (num `rem` 10, num `div` 10)) (0, num)
