#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
If you remove any letter from the word "boats you still get a word:

    boats
     oats => oats
    b ats => bats
    bo ts => bots
    boa s => boas
    boat  => boat

What is the longest English word to have this property?

This program was made with two sets of words, both taken as-is from

    https://github.com/dwyl/english-words/
-}

module Main where

import           Data.List   (find)
import           Debug.Trace

file :: FilePath
file = "words-alpha.txt"

main :: IO ()
main =
  readFile file >>= (putStrLn . show . filterSolutions . lines)

filterSolutions :: [String] -> [String]
filterSolutions words =
  aux words []

  where
    aux :: [String] -> [String] -> [String]
    aux [] acc = acc
    aux (h:t) acc =
      let newAcc = case isSolution 0 h of
                     Nothing  -> acc
                     Just wrd -> wrd:acc
      in
        aux t newAcc

    isSolution :: Int -> String -> Maybe String
    isSolution n wrd
      | n < length wrd =
          let subWord = delete n wrd
          in
            case find (\w -> w == subWord) words of
              Nothing -> Nothing
              Just _  -> isSolution (succ n) wrd
      | otherwise =
          Just wrd

delete :: Int -> [a] -> [a]
delete _ [] = []
delete n (h:t)
  | n == 0    = t
  | otherwise = h : delete (pred n) t
