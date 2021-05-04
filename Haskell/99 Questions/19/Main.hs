{-
Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples in Haskell:

    *Main> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"

    *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
-}

module Main where

rotate :: [a] -> Int -> [a]
rotate lst n
  | n == 0    = lst
  | n > 0     = rotate (tail lst ++ [head lst]) (n-1)
  | otherwise = rotate lst (length lst + n)

main :: IO ()
main = putStrLn "Hello, Haskell"
