{-
Find the K'th element of a list. The first element in the list is number 1.

Example in Haskell:

    Prelude> elementAt [1,2,3] 2
    2
    Prelude> elementAt "haskell" 5
    'e'
-}

module Main where

elementAt :: [a] -> Int -> a
elementAt lst n = lst !! (n - 1)

main :: IO ()
main = putStrLn "Hello, Haskell"
