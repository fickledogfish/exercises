#!/usr/bin/env stack
-- stack script --resolver lts-18.28

{-
                              21: Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n). If d(a) = b and d(b) = a, where a /= b, then a and
b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

Answer: 31626
-}

{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = print
  $ sum
  $ map (\x ->
     case x of
       Nothing -> 0
       Just a -> a)
  $ map hasAmicable [1 .. 10_000]

hasAmicable :: Int -> Maybe Int
hasAmicable n =
    if n == dnn && n /= dn then
        Just dn
    else
        Nothing

    where
        dn  = divsum n
        dnn = divsum dn

        divsum :: Int -> Int
        divsum = sum . divisors

        divisors :: Int -> [Int]
        divisors n = filter (\x -> n `mod` x == 0) [1 .. pred n]
