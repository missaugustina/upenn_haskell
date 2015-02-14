{-# OPTIONS_GHC -Wall #-}
module HW01 where
-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)
-- same as lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits x
 | x < 1 = []
 | otherwise = lastDigit x : (toRevDigits (dropLastDigit x))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:(y:zs)) = x : (y*2) : (doubleEveryOther zs)
doubleEveryOther xs = xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toRevDigits)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn =
    (f x) `mod` 10 == 0
    where f = sumDigits . doubleEveryOther . toRevDigits

-- could write this like this (but it would be hard to read):
-- luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [] -- no moves possible with an empty set
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
{-
1. move n − 1 discs from a to b using c as temporary storage
2. move the top disc
 from a to c
3. move n − 1 discs from b to c using a as temporary storage.
-}

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
