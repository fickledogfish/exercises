#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

Answer: 913 * 993 = 906609
-}

module Main where

main :: IO ()
main = print palindrome

  where
    (_, _, palindrome) = largestPalindrome 999 999

largestPalindrome :: Int -> Int -> (Int, Int, Int)
largestPalindrome cap1 cap2 =
  foldr larger (0, 0, 0) $ filter (\(_, _, n) -> isPalindrome n) allCombinations

  where
    allCombinations = [(x, y, x * y) | x <- [1..cap1], y <- [1..cap2]]

    larger l1@(_, _, p1) l2@(_, _, p2) = if p1 > p2 then l1 else l2

isPalindrome :: Int -> Bool
isPalindrome n =
  reversed == normal

  where
    normal = show n
    reversed = reverse normal
