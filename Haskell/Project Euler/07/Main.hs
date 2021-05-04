#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10 001st prime number?

Answer: 104743
-}

{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = print
  $ head
  $ listPrimes 10_001

listPrimes :: Int -> [Int]
listPrimes count =
  listPrimes' 3 [2]

  where
    listPrimes' :: Int -> [Int] -> [Int]
    listPrimes' currentNum primes =
      if length primes == count then
        primes
      else
        listPrimes' (currentNum + 2) $ newPrimes primes currentNum

    newPrimes :: [Int] -> Int -> [Int]
    newPrimes primes num =
      if not $ num `isDivisibleByAnyIn` primes then
        num:primes
      else
        primes

    isDivisibleByAnyIn :: Int -> [Int] -> Bool
    isDivisibleByAnyIn _ [] = False
    isDivisibleByAnyIn num (x:xs) =
      num `mod` x == 0 || isDivisibleByAnyIn num xs
