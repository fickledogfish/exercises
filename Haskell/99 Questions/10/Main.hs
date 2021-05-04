{-
Run-length encoding of a list. Use the result of problem P09 to implement the
so-called run-length encoding data compression method. Consecutive duplicates of
elements are encoded as lists (N E) where N is the number of duplicates of the
element E.

Example in Haskell:

    *Main> encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

module Main where

encode :: (Eq a) => [a] -> [(Int, a)]
encode s = [(length x, head x) | x <- pack s]
  where pack [] = []
        pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

main :: IO ()
main = putStrLn "Hello, Haskell"
