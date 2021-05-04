{-
Insert an element at a given position into a list.

Example in Haskell:

    P21> insertAt 'X' "abcd" 2
    "aXbcd"
-}

module Main where

insertAt :: a -> [a] -> Int -> [a]
insertAt el lst p = take (p-1) lst ++ [el] ++ drop (p-1) lst

main :: IO ()
main = putStrLn "Hello, Haskell"
