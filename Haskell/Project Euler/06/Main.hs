#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
The sum of the squares of the first ten natural numbers is,

    1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,

    (1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.

Answer: 25164150
-}

module Main where

main :: IO ()
main = print
  $ diff 100

diff :: Int -> Int
diff n = sqrsum n - sumsqr n

sumsqr :: Int -> Int
sumsqr n = sum $ map sqr [1..n]

sqrsum :: Int -> Int
sqrsum n = sqr $ sum [1..n]

sqr :: Int -> Int
sqr n = n * n
