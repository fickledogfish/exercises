#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

Answer: 142913828922
-}

{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Debug.Trace

main :: IO ()
main = print
  $ sum
  $ listPrimes 2_000_000
--  $ sieveEratosthenes 2_000_000

sieveEratosthenes :: Int -> [Int]
sieveEratosthenes cap =
  sieveEratosthenes' [3,5..cap] [2]

  where
    sieveEratosthenes' :: [Int] -> [Int] -> [Int]
    sieveEratosthenes' [] acc = acc
    sieveEratosthenes' (p:nums) acc =
      sieveEratosthenes' (filter (\n -> n `mod` p /= 0) nums) (p:acc)

listPrimes :: Int -> [Int]
listPrimes cap =
  listPrimes' 3 [2]

  where
    listPrimes' :: Int -> [Int] -> [Int]
    listPrimes' currentNum primes =
      if currentNum > cap then
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
