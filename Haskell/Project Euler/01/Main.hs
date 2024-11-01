#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

Answer: 233168
-}

module Main where

main :: IO ()
main = print $ solution 1000

solution :: Int -> Int
solution n = sum
           $ filter (\i -> i `mod` 3 == 0 || i `mod` 5 == 0)
           $ takeWhile (< n) [1..]
