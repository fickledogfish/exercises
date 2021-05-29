{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

module Main

isMultipleOfAny : List Int -> Int -> Bool
isMultipleOfAny [] _ = False
isMultipleOfAny (d1 :: ds) num =
  if num `mod` d1 == 0
     then True
     else isMultipleOfAny ds num

getMultiplesOf : List Int -> Int -> List Int
getMultiplesOf divs cap = aux divs (cap - 1) [] where
  aux : List Int -> Int -> List Int -> List Int
  aux _ 0 acc = acc
  aux divs cap acc =
    if isMultipleOfAny divs cap
       then aux divs (cap - 1) (cap :: acc)
       else aux divs (cap - 1) acc

main : IO ()
main = traverse_ putStrLn [
  "getMultiplesOf [3, 5] 10 = " ++ show (getMultiplesOf [3, 5] 10),
  "sum $ getMultiplesOf [3, 5] 10 = " ++ show (sum $ getMultiplesOf [3, 5] 10),
  "sum $ getMultiplesOf [3, 5] 1000 = " ++ show (sum $ getMultiplesOf [3, 5] 1000)
  ]
