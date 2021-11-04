#!/usr/bin/env stack
-- stack script --resolver lts-18.14

module Main where

import Text.Printf (printf)

input :: String
input = "10"

main :: IO ()
main = mapM_ putStrLn $ take (read input) $ map printNum [1 ..]
  where
    printNum :: Int -> String
    printNum n = printf "[%s] %2d" (if isOdd n then "X" else " ") n

    isOdd :: Int -> Bool
    isOdd n = (n `mod` 2) /= 0

    isEven :: Int -> Bool
    isEven n = (n `mod` 2) == 0

