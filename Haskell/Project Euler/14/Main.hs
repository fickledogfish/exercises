#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --extra-dep unordered-containers-0.2.9.0
-- stack --resolver lts-13.26 ghc Main.hs --package unordered-containers-0.2.9.0 -- -O3

{-
The following iterative sequence is defined for the set of positive integers:

    n -> n/2    (n is even)
    n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

    13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

Answer: 837_799
-}

{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Data.Array          (Array, bounds, inRange, listArray, (!))
import qualified Data.HashMap.Strict as HM
import           Data.List           (find, maximumBy, sortBy)

main :: IO ()
main =
  print
  $ memoizedSolution 1_000_000
--  $ optimizedSolution 1_000_000
--  $ naiveSolution 10_000

memoizedSolution :: Integer -> (Integer, Integer)
memoizedSolution cap =
  maximumBy (\(n, len) (n', len') -> compare len len')
  $ zip [1..cap] (map collatzLen [1..cap])

  where
    maxNumToMemoize :: Integer
    maxNumToMemoize = 10_000_000

    collatzseq :: Array Integer Integer
    collatzseq = listArray (1, maxNumToMemoize)
               $ map collatzLen [1..maxNumToMemoize]

    nextCollatz :: Integer -> Integer
    nextCollatz 1 = 1
    nextCollatz n = if even n then
                  n `div` 2
                else
                  3*n + 1

    collatzLen :: Integer -> Integer
    collatzLen 1 = 1
    collatzLen n =
      let next = nextCollatz n
      in if inRange (bounds collatzseq) next then
           succ $ collatzseq ! next
         else
           succ $ collatzLen next

type Size       = Int
type CollatzNum = Int
type CollatzSeq = [CollatzNum]
type CollatzMap = HM.HashMap CollatzNum CollatzSeq

optimizedSolution :: CollatzNum -> (CollatzNum, Size)
optimizedSolution cap =
  let (h:t) = maximumBy (\l1 l2 -> compare (length l1) (length l2))
            $ foldr collatz' HM.empty [1..cap]
  in
    (h, length t)

  where
    nextCollatz :: CollatzNum -> CollatzNum
    nextCollatz n
      | even n    = n `div` 2
      | otherwise = 3*n + 1

    collatz :: CollatzNum -> CollatzMap -> CollatzSeq
    collatz 1 hm = [1]
    collatz num hm = case HM.lookup num hm of
      Just seq -> num : seq
      Nothing  -> num : collatz (nextCollatz num) hm

    insertAllSubSeqs :: CollatzSeq -> CollatzMap -> CollatzMap
    insertAllSubSeqs []    hm   = hm
    insertAllSubSeqs [1]   hm   = hm
    insertAllSubSeqs l@(h:t) hm = insertAllSubSeqs t $ HM.insert h l hm

    collatz' :: CollatzNum -> CollatzMap -> CollatzMap
    collatz' 1 hm = hm
    collatz' n hm = insertAllSubSeqs (collatz n hm) hm

naiveSolution :: Size -> (CollatzNum, Size)
naiveSolution cap =
  maximumBy (\(n1, c1) (n2, c2) -> compare c1 c2)
  $ take cap
  $ map (\n -> (n, length $ collatz n)) [1..]

  where
    collatz :: CollatzNum -> [CollatzNum]
    collatz n =
      aux [n]

      where
        aux acc@(n:_)
          | n == 1 = acc
          | even n = aux (n `div` 2:acc)
          | odd n  = aux (3 * n + 1:acc)
