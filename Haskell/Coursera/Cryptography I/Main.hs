#!/usr/bin/env stack
-- stack script --resolver lts-13.26 --package binary --package bytestring

{-
Let us see what goes wrong when a stream cipher key is used more than
once. Below are eleven hex-encoded ciphertexts that are the result of encrypting
eleven plaintexts with a stream cipher, all with the same stream cipher
key. Your goal is to decrypt the last ciphertext, and submit the secret message
within it as solution.

Hint: XOR the ciphertexts together, and consider what happens when a space is
XORed with a character in [a-zA-Z].

For completeness, here is the python script used to generate the ciphertexts (it
doesn't matter if you can't read this)

    import sys

    MSGS = ( ---  11 secret messages  --- )

    def strxor(a, b):     # xor two strings of different lengths
        if len(a) > len(b):
           return "".join([chr(ord(x) ^ ord(y)) for (x, y) in zip(a[:len(b)], b)])
        else:
           return "".join([chr(ord(x) ^ ord(y)) for (x, y) in zip(a, b[:len(a)])])

    def random(size=16):
        return open("/dev/urandom").read(size)

    def encrypt(key, msg):
        c = strxor(key, msg)
        print
        print c.encode('hex')
        return c

    def main():
        key = random(1024)
        ciphertexts = [encrypt(key, msg) for msg in MSGS]
-}

module Main where

import           Data.Bits   (xor)
import           Data.List   (groupBy, sortBy)
import           Data.Maybe  (fromMaybe)
import           Numeric     (readHex, showHex)
import           Text.Printf (PrintfArg, printf)

printXor :: Int -> Int -> IO ()
printXor a b =
    printf "%08b\n%08b\n%08b\n" a b $ a `xor` b

filename :: String
filename = "ciphers.txt"

main :: IO ()
--main = readFile filename >>= printf "%x\n" . foldl xor 0 . knownCiphers . parseLines
--main = readFile filename >>= print . encodeHex . foldl xor 0 . knownCiphers . parseLines
--main = readFile filename >>= mapM_ (print . encodeHex) . knownCiphers . parseLines
main = do
    contents <- readFile filename
    let ciphers = map intToAscii $ knownCiphers $ parseLines contents
        c1      = head ciphers
        c2      = ciphers !! 1
        infZero = repeat $ toEnum 0
        --sortLen = sortBy (\a b -> length b `compare` length a)
        group   = groupBy (\a b -> snd a == snd b) . sortBy (\a b -> (snd a) `compare` (snd b))
        sortLen = sortBy (\a b -> length b `compare` length a)
        xorAll  = foldr xorText infZero ciphers
        indexed = zip [0..]

        -- [(Index, Code)] -> (Code, Count, [Index])
        count lst = (snd $ head lst, length lst, map fst lst)
      in
      --print $ sortLen $ group $ sort $ map fromEnum $ xorText c1 c2
      mapM_ print $ map count $ sortLen $ group $ indexed $ map fromEnum $ xorAll
      --mapM_ print $ map count $ sortLen $ group $ indexed $ map fromEnum $ xorText c1 c2

xorText :: String -> String -> String
--xorText text key
--  | length text /= length key =
--      error "Mismatched lengths for key and message"
--
--  | otherwise =
--      zipWith xorChar text key
xorText = zipWith xorChar

xorChar :: Char -> Char -> Char
xorChar plain key = toEnum $ xor (fromEnum plain) (fromEnum key)

intToAscii :: (Integral a, PrintfArg a) => a -> String
intToAscii c = map (toEnum . decodeHex) $ chunksOf 2 $ printf "%x" c

chunksOf :: Show a => Int -> [a] -> [[a]]
chunksOf n lst =
  aux 1 lst []

  where
    aux :: Show a => Int -> [a] -> [a] -> [[a]]
    aux c [h] acc = [reverse $ h:acc]
    aux c (h:t) acc
      | c `mod` n == 0 = reverse (h:acc) : aux (succ c) t []
      | otherwise = aux (succ c) t (h:acc)

parseLines :: String -> ParsedLines
parseLines ls =
    ParsedLines { linecount    = read num
                , targetCipher = decodeHex target
                , knownCiphers = map decodeHex ciphers
                }

    where
        (num:target:ciphers) = filter (/= []) $ lines ls

data ParsedLines = ParsedLines
    { linecount    :: Int
    , targetCipher :: Integer
    , knownCiphers :: [Integer]
    } deriving(Show)

encodeHex :: (Integral a, Show a) => a -> String
encodeHex = (`showHex` "")

decodeHex :: Integral a => String -> a
decodeHex hstr =
  if length parsed == 1 then
    fst $ head parsed
  else
    error "Failed to parse hex string"

  where
    parsed = readHex hstr
