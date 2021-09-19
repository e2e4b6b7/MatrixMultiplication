module StrangeForms (runTests) where

import Lib
import Test.Tasty.HUnit (Assertion, (@?=))

matrix :: Int -> Int -> Matrix Int
matrix = emptyMatrix

runTests :: Assertion
runTests = do
  testOdd
  testVectors

testOdd :: Assertion
testOdd = do
  let m3x5 = matrix 3 5 <<# [(1, 2, 4), (4, 2, 1), (-3, 1, 1)]
  let m3x5' = matrix 3 5 <<# [(2, 0, 0), (3, 0, 1), (3, 1, 1)]
  let m3x5sum = matrix 3 5 <<# [(2, 0, 0), (3, 0, 1), (1, 2, 4), (4, 2, 1)]
  True @?= (m3x5 +# m3x5' == m3x5sum)
  False @?= (m3x5 +# m3x5' == m3x5)
  False @?= (m3x5 +# m3x5' == m3x5')
  let m5x3 = matrix 5 3 <<# [(3, 4, 2), (1, 0, 0), (-1, 2, 1)]
  let mult1 = matrix 5 5 <<# [(2, 0, 0), (3, 0, 1), (12, 4, 1), (3, 4, 4)]
  False @?= (m5x3 == mult1)
  True @?= (m3x5sum *# m5x3 == mult1)
  let mult2 = matrix 3 3 <<# [(2, 0, 0), (3, 2, 2)]
  True @?= (m5x3 *# m3x5sum == mult2)

testVectors :: Assertion
testVectors = do
  let v8 = matrix 1 8 <<# [(3, 0, 1), (2, 0, 3)]
  let v8' = matrix 1 8 <<# [(4, 0, 2), (-5, 0, 6)]
  let v8sum = matrix 1 8 <<# [(3, 0, 1), (2, 0, 3), (4, 0, 2), (-5, 0, 6)]
  True @?= (v8 +# v8' == v8sum)
