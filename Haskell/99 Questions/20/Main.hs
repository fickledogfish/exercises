{-
Remove the K'th element from a list.

Example in Haskell:

    *Main> removeAt 2 "abcd"
    ('b',"acd")
-}

module Main where

removeAt :: Int -> [a] -> (a, [a])
removeAt n lst = (lst !! (n-1), take (n-1) lst ++ drop n lst)

main :: IO ()
main = putStrLn "Hello, Haskell"
