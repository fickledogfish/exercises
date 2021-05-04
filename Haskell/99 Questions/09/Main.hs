{-
Pack consecutive duplicates of list elements into sublists. If a list contains
repeated elements they should be placed in separate sublists.

Example in Haskell:

    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
                 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}

module Main where

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = h : t
  where h = (x : takeWhile (== x) xs)
        t = pack (dropWhile (== x) xs)

main :: IO ()
main = putStrLn "Hello, Haskell"
