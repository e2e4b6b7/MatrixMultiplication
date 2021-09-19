module Lib
  ( Matrix,
    emptyMatrix,
    setVal,
    matrixSum,
    matrixMultiplication,
    (<#),
    (<<#),
    (+#),
    (*#),
  )
where

import Data.Semiring (Semiring (plus, times, zero))

data Matrix a = Matrix
  { width :: Int,
    height :: Int,
    values :: MatrixValues a
  }
  deriving (Eq, Show)

data MatrixValues a
  = Zero
  | NonZero
      { ul :: MatrixValues a,
        ur :: MatrixValues a,
        dl :: MatrixValues a,
        dr :: MatrixValues a
      }
  | NonZeroV
      { u :: MatrixValues a,
        d :: MatrixValues a
      }
  | NonZeroH
      { l :: MatrixValues a,
        r :: MatrixValues a
      }
  | Leaf a
  deriving (Eq, Show)

infixl 5 <#

infixl 5 <<#

infixl 6 +#

infixl 7 *#

(<#) :: (Show a, Eq a, Semiring a) => Matrix a -> (a, Int, Int) -> Matrix a
matrix <# (val, w, h) = setVal matrix val w h

(<<#) :: (Show a, Eq a, Semiring a) => Matrix a -> [(a, Int, Int)] -> Matrix a
(<<#) = foldl (<#)

(*#) :: (Show a, Eq a, Semiring a) => Matrix a -> Matrix a -> Matrix a
(*#) = matrixMultiplication

(+#) :: (Show a, Eq a, Semiring a) => Matrix a -> Matrix a -> Matrix a
(+#) = matrixSum

leaf :: (Show a, Eq a, Semiring a) => a -> MatrixValues a
leaf val
  | val == zero = Zero
  | otherwise = Leaf val

nonZero :: (Show a, Eq a, Semiring a) => MatrixValues a -> MatrixValues a -> MatrixValues a -> MatrixValues a -> MatrixValues a
nonZero ul ur dl dr
  | ul == Zero && ur == Zero && dl == Zero && dr == Zero = Zero
  | otherwise = NonZero ul ur dl dr

nonZeroH :: (Show a, Eq a, Semiring a) => MatrixValues a -> MatrixValues a -> MatrixValues a
nonZeroH l r
  | l == Zero && r == Zero = Zero
  | otherwise = NonZeroH l r

nonZeroV :: (Show a, Eq a, Semiring a) => MatrixValues a -> MatrixValues a -> MatrixValues a
nonZeroV u d
  | u == Zero && d == Zero = Zero
  | otherwise = NonZeroV u d

emptyMatrix :: Int -> Int -> Matrix a
emptyMatrix w h = Matrix w h Zero

setVal :: (Show a, Eq a, Semiring a) => Matrix a -> a -> Int -> Int -> Matrix a
setVal matrix@(Matrix width height values) val w h
  | w < 0 || w >= width || h < 0 || h >= height = error "Position not in matrix"
  | otherwise = matrix {values = setValValues values val w h width height}

setValValues :: (Show a, Eq a, Semiring a) => MatrixValues a -> a -> Int -> Int -> Int -> Int -> MatrixValues a
setValValues values@(NonZero ul ur dl dr) val w h width height
  | w < midW && h < midH = values {ul = setValValues ul val w h midW midH}
  | w >= midW && h < midH = values {ur = setValValues ur val (w - midW) h (width - midW) midH}
  | w < midW && h >= midH = values {dl = setValValues dl val w (h - midH) midW (height - midH)}
  | w >= midW && h >= midH = values {dr = setValValues dr val (w - midW) (h - midH) (width - midW) (height - midH)}
  where
    midW = width `div` 2
    midH = height `div` 2
setValValues values@(NonZeroV u d) val 0 h 1 height
  | h < midH = values {u = setValValues u val 0 h 1 midH}
  | h >= midH = values {d = setValValues d val 0 (h - midH) 1 (height - midH)}
  where
    midH = height `div` 2
setValValues values@(NonZeroH l r) val w 0 width 1
  | w < midW = values {l = setValValues l val w 0 midW 1}
  | w >= midW = values {r = setValValues r val (w - midW) 0 (width - midW) 1}
  where
    midW = width `div` 2
setValValues Zero val 0 0 1 1 = Leaf val
setValValues Zero val 0 h 1 height = setValValues (NonZeroV Zero Zero) val 0 h 1 height
setValValues Zero val w 0 width 1 = setValValues (NonZeroH Zero Zero) val w 0 width 1
setValValues Zero val w h width height = setValValues (NonZero Zero Zero Zero Zero) val w h width height
setValValues (Leaf _) val 0 0 1 1 = Leaf val
setValValues _ _ _ _ _ _ = error "Unexpected matrix structure"

matrixSum :: (Show a, Eq a, Semiring a) => Matrix a -> Matrix a -> Matrix a
matrixSum (Matrix w1 h1 values1) (Matrix w2 h2 values2)
  | w1 /= w2 || h1 /= h2 = error "Matrices are not the same size"
  | otherwise = Matrix w1 h1 $ matrixSumValues values1 values2

matrixSumValues :: (Show a, Eq a, Semiring a) => MatrixValues a -> MatrixValues a -> MatrixValues a
matrixSumValues a Zero = a
matrixSumValues Zero a = a
matrixSumValues (Leaf a) (Leaf b) = leaf $ a `plus` b
matrixSumValues (NonZero ul1 ur1 dl1 dr1) (NonZero ul2 ur2 dl2 dr2) =
  nonZero
    (matrixSumValues ul1 ul2)
    (matrixSumValues ur1 ur2)
    (matrixSumValues dl1 dl2)
    (matrixSumValues dr1 dr2)
matrixSumValues (NonZeroV l1 r1) (NonZeroV l2 r2) =
  nonZeroV
    (matrixSumValues l1 l2)
    (matrixSumValues r1 r2)
matrixSumValues (NonZeroH u1 d1) (NonZeroV u2 d2) =
  nonZeroH
    (matrixSumValues u1 u2)
    (matrixSumValues d1 d2)
matrixSumValues _ _ = error "Mismatch of matrix structures"

matrixMultiplication :: (Show a, Eq a, Semiring a) => Matrix a -> Matrix a -> Matrix a
matrixMultiplication (Matrix w1 h1 values1) (Matrix w2 h2 values2)
  | w1 /= h2 = error "Matrix sizes are not suitable for multiplication"
  | otherwise = Matrix w2 h1 $ matrixMultiplicationValues values1 values2

matrixMultiplicationValues :: (Show a, Eq a, Semiring a) => MatrixValues a -> MatrixValues a -> MatrixValues a
matrixMultiplicationValues _ Zero = Zero
matrixMultiplicationValues Zero _ = Zero
matrixMultiplicationValues (Leaf a) (Leaf b) = leaf $ a `times` b
matrixMultiplicationValues (NonZero ul1 ur1 dl1 dr1) (NonZero ul2 ur2 dl2 dr2) =
  nonZero
    (matrixSumValues (matrixMultiplicationValues ul1 ul2) (matrixMultiplicationValues ur1 dl2))
    (matrixSumValues (matrixMultiplicationValues ul1 ur2) (matrixMultiplicationValues ur1 dr2))
    (matrixSumValues (matrixMultiplicationValues dl1 ul2) (matrixMultiplicationValues dr1 dl2))
    (matrixSumValues (matrixMultiplicationValues dl1 ur2) (matrixMultiplicationValues dr1 dr2))
matrixMultiplicationValues (NonZeroV ul1 dl1) (NonZeroH ul2 ur2) =
  nonZero
    (matrixMultiplicationValues ul1 ul2)
    (matrixMultiplicationValues ul1 ur2)
    (matrixMultiplicationValues dl1 ul2)
    (matrixMultiplicationValues dl1 ur2)
matrixMultiplicationValues (NonZeroH ul1 ur1) (NonZeroV ul2 dl2) =
  matrixSumValues (matrixMultiplicationValues ul1 ul2) (matrixMultiplicationValues ur1 dl2)
matrixMultiplicationValues (NonZeroH ul1 ur1) (NonZero ul2 ur2 dl2 dr2) =
  nonZeroH
    (matrixSumValues (matrixMultiplicationValues ul1 ul2) (matrixMultiplicationValues ur1 dl2))
    (matrixSumValues (matrixMultiplicationValues ul1 ur2) (matrixMultiplicationValues ur1 dr2))
matrixMultiplicationValues (NonZero ul1 ur1 dl1 dr1) (NonZeroV ul2 dl2) =
  nonZeroV
    (matrixSumValues (matrixMultiplicationValues ul1 ul2) (matrixMultiplicationValues ur1 dl2))
    (matrixSumValues (matrixMultiplicationValues dl1 ul2) (matrixMultiplicationValues dr1 dl2))
matrixMultiplicationValues leafVal@(Leaf _) (NonZeroH ul2 ur2) =
  nonZeroH (matrixMultiplicationValues leafVal ul2) (matrixMultiplicationValues leafVal ur2)
matrixMultiplicationValues (NonZeroV ul1 dl1) leafVal@(Leaf _) =
  nonZeroV (matrixMultiplicationValues ul1 leafVal) (matrixMultiplicationValues dl1 leafVal)
matrixMultiplicationValues _ _ = error "Mismatch of matrix structures"
