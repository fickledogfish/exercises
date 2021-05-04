{-
Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example in Haskell:

    *Main> split "abcdefghik" 3
    ("abc", "defghik")
-}

module Main where

split :: [a] -> Int -> ([a], [a])
split lst n = (take n lst, drop n lst)

main :: IO ()
main = putStrLn "Hello, Haskell"
