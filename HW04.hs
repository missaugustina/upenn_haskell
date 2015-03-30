{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------
-- 1x + 0
x :: Num a => Poly a
x = P [0, 1]
-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P (y:ys) == P (z:zs)
        | y == z = P ys == P zs
        | otherwise = False
    P [] == P [] = True
    P [] == P (0:zs) = P [] == P zs
    P (0:ys) == P [] = P [] == P ys
    _ == _ = False

-- Exercise 3 -----------------------------------------
-- Terms are displayed as cx^e where c is the coefficient and e is the
-- exponent. If e is 0, then only the coefficient is displayed. If e is 1
-- then the format is simply cx.

-- 3x^2 + 2x + 1
-- Example: show (P [1, 0, 0, 2]) == "2x^3 + 1"
-- Example: show (P [0, -1, 2]) == "2x^2 + -x"

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P ns) = joinResult $ formatPairs $ filterPairs ns

filterPairs :: (Eq a, Num a) => [a] -> [(a, Int)]
filterPairs ns = reverse $ filter (\n -> (fst n) /= 0) $ zip ns [0..(length ns)]

formatPairs :: (Show a, Show a1, Num a1, Eq a1, Num a, Eq a) => [(a, a1)] -> [[Char]]
formatPairs [] = ["0"]
formatPairs ps = map format ps
    where
      format (1,1) = "x"
      format (-1,1) = "-x"
      format (n,0) = (show n)
      format (n,1) = (show n) ++ "x"
      format (-1,e) = "-x^" ++ (show e)
      format (1,e) = "x^" ++ (show e)
      format (n,e) = (show n) ++ "x^" ++ (show e)

joinResult :: [[Char]] -> [Char]
joinResult fps = intercalate " + " fps

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
