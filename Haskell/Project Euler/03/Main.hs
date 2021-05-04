#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?

Answer: 6857
-}

module Main where

main :: IO ()
main = print
     $ foldr max 0
     $ notSoTrivialDivision 600851475143

{-
Given a number n, return a list of its prime factors.

Solution by trivial division:

    - if the passed number is less or equal to 1, we tested everything we could,
      so return the reversed accumulated list

    - check if the currGuess is a factor of n

        - if it is, add it to the accumulator, and continue testing by passing
          the result of the division of the desired number by the factor as the
          new desired number

        - if it isn't, make currGuess the next number and try again
-}
trivialDivision :: Int -> [Int]
trivialDivision n =
  primeFactorsOf n 2 []

  where
    primeFactorsOf :: Int -> Int -> [Int] -> [Int]
    primeFactorsOf n currGuess acc
      | n <= 1 =
        reverse acc

      | n `mod` currGuess == 0 =
        primeFactorsOf (n `div` currGuess) (next currGuess) (currGuess:acc)

      | otherwise =
        primeFactorsOf n (succ currGuess) acc

    next :: Int -> Int
    next = succ

{-
A small improvement of the trivialDivision solution. Checks only once against
even numbers.
-}
notSoTrivialDivision :: Int -> [Int]
notSoTrivialDivision n =
  primeFactorsOf n 3 [2 | even n]

  where
    primeFactorsOf :: Int -> Int -> [Int] -> [Int]
    primeFactorsOf n currGuess acc
      | n <= 1 =
        reverse acc

      | n `mod` currGuess == 0 =
        primeFactorsOf (n `div` currGuess) (next currGuess) (currGuess:acc)

      | otherwise =
        primeFactorsOf n (currGuess + 2) acc

    next :: Int -> Int
    next = (+ 2)
