{-
Drop every N'th element from a list.

Example in Haskell:

    *Main> dropEvery "abcdefghik" 3
    "abdeghk"
-}

module Main where

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery s n = take (n-1) s ++ dropEvery (drop n s) n

main :: IO ()
main = putStrLn "Hello, Haskell"
