#!/usr/bin/env stack
-- stack script --resolver lts-13.26

{-
                              19: Counting Sundays

You are given the following information, but you may prefer to do some research
for yourself.

    - 1 Jan 1900 was a Monday.

    - Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.

    - A leap year occurs on any year evenly divisible by 4, but not on a century
      unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1
Jan 1901 to 31 Dec 2000)?

Answer: 171
-}

module Main where

import           Data.Monoid (All (All), getAll)
import           Text.Printf (printf)

main :: IO ()
main = print
  $ length
  $ filter problemPredicates
  $ takeWhile (\d -> year d < lastYear)
  $ iterate nextDay firstKnownDay

problemPredicates :: Date -> Bool
problemPredicates =
  -- Totally not stolen from Tsoding.
  getAll . foldMap (All .) predicates

  where
    predicates :: [Date -> Bool]
    predicates = [ \Date {year = y}     -> y   >= startingYear
                 , \Date {day = day}    -> day == 1
                 , \Date {weekday = wd} -> wd  == Sunday
                 ]

nextDay :: Date -> Date
nextDay Date { year    = y
             , month   = m
             , day     = d
             , weekday = wd
             }
  = Date { year    = nextYear
         , month   = nextMonth
         , day     = nextDay
         , weekday = succ wd
         }

  where
    (nextDay, changeMonth) =
      if d == daysInMonth m (isLeapYear y) then
        (1, True)
      else
        (succ d, False)

    (nextMonth, changeYear) =
      if changeMonth then
        (succ m, m == December)
      else
        (m, False)

    nextYear =
      if changeYear then
        succ y
      else
        y

lastYear :: Int
lastYear = 2001

startingYear :: Int
startingYear = 1901

firstKnownDay :: Date
firstKnownDay =
  Date { year    = 1900
       , month   = January
       , day     = 1
       , weekday = Monday
       }

data Date =
  Date { year    :: Int
       , month   :: Month
       , day     :: Int
       , weekday :: WeekDay
       }

instance Show Date where
  show Date { year = y, month = m, day = d, weekday = wd } =
    printf "%04d-%02d-%02d (%s)" y (succ $ fromEnum m) d (show wd)

isLeapYear :: Int -> Bool
isLeapYear y
  | y `mod` 4 == 0 = not $ y `mod` 100 == 0 && y `mod` 400 /= 0
  | otherwise = False

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
  deriving(Eq, Show)

instance Enum WeekDay where
  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday

  fromEnum Sunday    = 0
  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6

daysInMonth :: Month -> Bool -> Int
daysInMonth January   _     = 31
daysInMonth February  True  = 29
daysInMonth February  False = 28
daysInMonth March     _     = 31
daysInMonth April     _     = 30
daysInMonth May       _     = 31
daysInMonth June      _     = 30
daysInMonth July      _     = 31
daysInMonth August    _     = 31
daysInMonth September _     = 30
daysInMonth October   _     = 31
daysInMonth November  _     = 30
daysInMonth December  _     = 31

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
  deriving(Eq)

instance Enum Month where
  toEnum 0  = January
  toEnum 1  = February
  toEnum 2  = March
  toEnum 3  = April
  toEnum 4  = May
  toEnum 5  = June
  toEnum 6  = July
  toEnum 7  = August
  toEnum 8  = September
  toEnum 9  = October
  toEnum 10 = November
  toEnum 11 = December
  toEnum 12 = January

  fromEnum January   = 0
  fromEnum February  = 1
  fromEnum March     = 2
  fromEnum April     = 3
  fromEnum May       = 4
  fromEnum June      = 5
  fromEnum July      = 6
  fromEnum August    = 7
  fromEnum September = 8
  fromEnum October   = 9
  fromEnum November  = 10
  fromEnum December  = 11
