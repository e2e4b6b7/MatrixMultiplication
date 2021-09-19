module SmallMatrices (runTests) where

import Lib
import Test.Tasty.HUnit (Assertion, (@?=))

empty :: Matrix Int
empty = emptyMatrix 2 2

identity :: Matrix Int
identity =
  empty
    <# (1, 0, 0)
    <# (1, 1, 1)

runTests :: Assertion
runTests = do
  testSum
  testMultiplication

testSum :: Assertion
testSum = do
  let matrix2 = empty <# (1, 0, 0)
  let matrix3 = empty <# (1, 1, 1)
  True @?= (identity == matrixSum matrix2 matrix3)

testMultiplication :: Assertion
testMultiplication = do
  let matrix = empty <# (3, 0, 0) <# (4, 0, 1) <# (7, 1, 0)
  let matrixPow2 = empty <<# [(37, 0, 0), (12, 0, 1), (21, 1, 0), (28, 1, 1)]
  True @?= (matrix == matrixMultiplication identity matrix)
  True @?= (matrix == matrixMultiplication matrix identity)
  let matrixPow2Calc = matrixMultiplication matrix matrix
  False @?= (matrix == matrixPow2Calc)
  True @?= (matrixPow2 == matrixPow2Calc)
