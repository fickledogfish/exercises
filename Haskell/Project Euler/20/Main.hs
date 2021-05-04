#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                            20: Factorial digit sum

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, and the sum of the digits
in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

Answer: 648
-}

module Main where

main :: IO ()
main = print
  $ sum
  $ digits
  $ factorial 100

digits :: Integer -> [Integer]
digits n =
  aux n []

  where
    aux :: Integer -> [Integer] -> [Integer]
    aux n acc
      | n < 10    = n : acc
      | otherwise = aux (n `div` 10) $ n `rem` 10 : acc

factorial :: Integer -> Integer
factorial n = product [2..n]
