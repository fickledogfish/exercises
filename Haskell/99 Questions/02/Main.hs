{-
Find the last but one element of a list.

Example in Haskell:

    Prelude> myButLast [1,2,3,4]
    3
    Prelude> myButLast ['a'..'z']
    'y'
-}

module Main where

myButLast :: [a] -> a
myButLast lst = reverse lst !! 1

main :: IO ()
main = putStrLn "Hello, Haskell"
