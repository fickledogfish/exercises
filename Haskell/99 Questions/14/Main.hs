{-
Duplicate the elements of a list.

Example in Haskell:

    > dupli [1, 2, 3]
    [1,1,2,2,3,3]
-}

module Main where

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs

main :: IO ()
main = putStrLn "Hello, Haskell"
