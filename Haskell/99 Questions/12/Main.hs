{-
Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its
uncompressed version.

Example in Haskell:

    P12> decodeModified
           [Multiple 4 'a',Single 'b',Multiple 2 'c',
            Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"
-}

module Main where

data EncodeElement a n = Single a | Multiple n a
  deriving (Show)

{-
-- First Solution

decodeModified [] = []
decodeModified [(Single c)] = [c]
decodeModified [(Multiple n x)] = n `replicate` x
decodeModified (x:xs) = (decodeModified [x]) ++ (decodeModified xs)
-}

decodeModified :: [EncodeElement a Int] -> [a]
decodeModified lst = concatMap decode lst
  where decode (Single c) = [c]
        decode (Multiple n x) = n `replicate` x

main :: IO ()
main = putStrLn "Hello, Haskell"
