module MainTest where

import Test.HUnit
import Main

test1 = TestCase (assertEqual "" 4 (myLast [1, 2, 3, 4]))

test2 = TestCase (assertEqual "" 'z' (myLast ['x','y','z']))

tests = TestList [
  TestLabel "test1" test1,
  TestLabel "test2" test2
  ]
