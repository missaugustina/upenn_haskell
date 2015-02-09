{-# OPTIONS_GHC -Wall #-}
module HW01 where
-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits x =
 if x < 1
 then []
 else
   let
    loop y =
      if z == y
      then [z]
      else z : (loop (dropLastDigit y))
      where z = lastDigit y
   in loop x

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : (y*2) : (doubleEveryOther zs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
 if lastDigit x == x
 then x + sumDigits xs
 else (sum (toRevDigits x)) + sumDigits xs



-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x =
  if y `mod` 10 == 0
  then True
  else False
  where y = sumDigits(doubleEveryOther(toRevDigits x))



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
