{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------
-- 1x + 0
x :: Num a => Poly a
x = P [0, 1]
-- Exercise 2 ----------------------------------------
-- derive equal
instance (Num a, Eq a) => Eq (Poly a) where
    P (y:ys) == P (z:zs)
        | y == z = P ys == P zs
        | otherwise = False
    P [] == P [] = True
    P [] == P (0:zs) = P [] == P zs
    P (0:ys) == P [] = P [] == P ys
    _ == _ = False

-- Exercise 3 -----------------------------------------
-- derive show
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
-- Example: P [-5, 0, 1] + P [1, 1, 2] == P [-4, 1, 3]
-- Example: P [1, 0, 1] + P [1] == P [2, 0, 1]
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) =
    P (zipWithEmpty p1 p2)
    where
      zipWithEmpty :: Num a => [a] -> [a] -> [a]
      zipWithEmpty (a:as) (b:bs) = a + b : zipWithEmpty as bs
      zipWithEmpty [] (b:bs) = b : zipWithEmpty [] bs
      zipWithEmpty (a:as) [] = a : zipWithEmpty as []
      zipWithEmpty [] [] = []

-- Exercise 5 -----------------------------------------
-- Example: P [1, 1, 1] * P [2, 2] == P [2, 4, 4, 2]
-- P [1, 1, 1] * P [2, 2] will yield the list
--  [P [2, 2], P [0, 2, 2], P [0, 0, 2, 2]].
-- You can then simply sum this list

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
