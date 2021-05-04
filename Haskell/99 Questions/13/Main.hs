{-
Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method
directly. I.e. don't explicitly create the sublists containing the duplicates,
as in problem 9, but only count them. As in problem 11, simplify the result list
by replacing the singleton lists (1 X) by X.

Example in Haskell:

    > encodeDirect "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

module Main where

data EncodeElement a n = Single a
                       | Multiple n a
  deriving (Show)

encodeDirect :: (Eq a) => [a] -> [EncodeElement a Int]
encodeDirect [] = []
encodeDirect (x:xs) = encodeHelper x 1 xs
  where
    encodeHelper curr cnt [] = [encode cnt curr]
    encodeHelper curr cnt (x:xs)
      | curr == x = encodeHelper curr (cnt + 1) xs
      | otherwise = (encode cnt curr) : (encodeHelper x 1 xs)

    encode 1 el = Single el
    encode cnt el = Multiple cnt el

main :: IO ()
main = putStrLn "Hello, Haskell"
