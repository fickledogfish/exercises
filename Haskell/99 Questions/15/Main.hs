{-
Replicate the elements of a list a given number of times.

Example in Haskell:

    > repli "abc" 3
    "aaabbbccc"
-}

module Main where

repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = (n `replicate` x) ++ (repli xs n)

main :: IO ()
main = putStrLn "Hello, Haskell"
