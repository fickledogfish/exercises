#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                              21: Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n). If d(a) = b and d(b) = a, where a /= b, then a and
b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

Answer:
-}

module Main where

main :: IO ()
main = print
  $ divisors 10 []

hasAmicable :: Int -> Maybe Int
hasAmicable n = Nothing

divisors :: Int            -- | The number
         -> [Int]          -- | A list of known prime numbers
         -> ([Int], [Int]) -- | Potentialli a new list of primes and the prime
                           --   factors of n
divisors n ps
  | n `elem` ps = (ps, [1, n])
  | n < last ps = undefined

primes :: [Int]
primes = [1, 2, primes]
