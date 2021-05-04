{-
Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no
duplicates it is simply copied into the result list. Only elements with
duplicates are transferred as (N E) lists.

Example in Haskell:

    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a', Single 'b', Multiple 2 'c',
     Multiple 2 'a', Single 'd', Multiple 4 'e']
-}

module Main where

data EncodeElement a n = Single a | Multiple n a
  deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodeElement a Int]
encodeModified s = [if length x > 1 then
                      (Multiple (length x) (head x))
                    else
                      Single (head x)
                   | x <- pack s]
  where
    pack :: (Eq a) => [a] -> [[a]]
    pack [] = []
    pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

main :: IO ()
main = putStrLn "Hello, Haskell"
