#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                            17: Number letter counts

If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with British
usage.

Answer: 21124
-}

module Main where

main :: IO ()
main =
  print
  $ length
  $ concatMap (ignoreCharacters . digitsToString . toDigits) [1..1000]

ignoreCharacters :: String -> String
ignoreCharacters =
  filter (`notElem` ignoreList)

  where
    ignoreList :: String
    ignoreList = [' ', '-']

digitsToString :: [Int] -> String
digitsToString lst =
  concat $ interlace " " $ reverse $ aux False 1 $ reverse lst

  where
    aux :: Bool -> Int -> [Int] -> [String]
    aux _ _ [] = []
    aux _ mag (0:ns) = aux False (10*mag) ns
    aux _ 1 (n1:n2:ns)
      | n2 > 1    = stringfyWithHyphen n1 n2 : aux True 100 ns
      | otherwise = numToString (n1 + 10*n2) : aux True 100 ns
    aux insertAnd mag (n:ns)
      | mag == 100 || mag == 1000 =
          magToString mag insertAnd : numToString n : aux insertAnd (10*mag) ns
      | otherwise =
          numToString (mag*n) : aux True (10*mag) ns

    magToString :: Int -> Bool -> String
    magToString 100 insertAnd = "hundred" ++ if insertAnd then " and" else ""
    magToString 1000 _        = "thousand"

    stringfyWithHyphen :: Int -> Int -> String
    stringfyWithHyphen n1 n2 = numToString (10*n2) ++ "-" ++ numToString n1

interlace :: a -> [a] -> [a]
interlace _ [e] = [e]
interlace sep (e:es) =
  e : sep : interlace sep es

numToString :: Int -> String
numToString 1  = "one"
numToString 2  = "two"
numToString 3  = "three"
numToString 4  = "four"
numToString 5  = "five"
numToString 6  = "six"
numToString 7  = "seven"
numToString 8  = "eight"
numToString 9  = "nine"
numToString 10 = "ten"
numToString 11 = "eleven"
numToString 12 = "twelve"
numToString 13 = "thirteen"
numToString 14 = "fourteen"
numToString 15 = "fifteen"
numToString 16 = "sixteen"
numToString 17 = "seventeen"
numToString 18 = "eighteen"
numToString 19 = "nineteen"
numToString 20 = "twenty"
numToString 30 = "thirty"
numToString 40 = "forty"
numToString 50 = "fifty"
numToString 60 = "sixty"
numToString 70 = "seventy"
numToString 80 = "eighty"
numToString 90 = "ninety"

toDigits :: Int -> [Int]
toDigits = reverse . toDigitsRev

toDigitsRev :: Int -> [Int]
toDigitsRev 0 = []
toDigitsRev n = last : toDigitsRev rest
  where
    (rest, last) = n `quotRem` 10
