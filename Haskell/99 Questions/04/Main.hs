{-
Find the number of elements of a list.

Example in Haskell:

    Prelude> myLength [123, 456, 789]
    3
    Prelude> myLength "Hello, world!"
    13
-}

module Main where

myLength :: [a] -> Integer
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

main :: IO ()
main = putStrLn "Hello, Haskell"
