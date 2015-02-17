{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
-- first try:
exactMatches' :: Code -> Code -> Int
exactMatches' = loop 0
    where loop acc (x:xs) (y:ys)
             | x == y = loop (acc+1) xs ys
             | otherwise = loop acc xs ys
          loop acc _ _ = acc

-- second try:
exactMatches :: Code -> Code -> Int
exactMatches code guess =
    length . filter (uncurry (==)) $ zip code guess -- have == take pairs

-- exactMatches' [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] == 0
-- exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] == 2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors pegs =
    map f colors
    where f c = length . filter (== c) $ pegs

-- rafl's solution
countColors' :: Code -> [Int]
countColors' code = map (exactMatches code . repeat) colors

-- Count number of matches between the actual code and the guess
-- first attempt
matches' :: Code -> Code -> Int
matches' code guess =
     sum . map (uncurry min) $ zip (countColors code) (countColors guess)
-- uncurry because zip returns pairs and min takes two separate args,
-- so we need min to take a pair
-- $ = apply function on the left to arg on right
-- map is only partially applied, so we can compose it w/ point-free

matches :: Code -> Code -> Int
matches code guess =
    sum $ zipWith min (countColors code) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess =
    Move guess exact (allMatches - exact)
    where
      exact = exactMatches secret guess
      allMatches = matches secret guess

    -- return guess, exact, non-exact

-- Exercise 4 -----------------------------------------
-- does the possible code match the result of comparing the move
-- with the actual secret code? If yes, it's consistent.
isConsistent :: Move -> Code -> Bool
isConsistent move@(Move c _ _) possibility =
    compareMoves move (getMove c possibility)
    where compareMoves (Move _ e1 f1) (Move _ e2 f2) = (e1 == e2) && (f1 == f2)

-- Exercise 5 -----------------------------------------
-- given a list of codes, filter out the inconsistent ones
filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)
-- could remove "move" and parents around isConsistent and replace with "."

-- Exercise 6 -----------------------------------------
-- given a length, return a list of all possible codes of that length
-- colors = [Red, Green, Blue, Yellow, Orange, Purple] - 6
allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\x -> [x]) colors
allCodes n = concatMap f $ allCodes (n-1)
             where f code = map (\x -> code ++ [x]) colors

-- map all colors to the provided code
-- [Red, Red] -> [[Red, Red, Red],[Red, Red, Green]...]
    -- concatMap f colors
-- takes all the codes of length n − 1 and produces all codes of length n

-- allCodes 1 == [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
-- allCodes 2 == [[Red, Red], [Red, Green], [Red, Blue]...

-- Exercise 7 -----------------------------------------
-- start with [Red, Red, ..]
solve :: Code -> [Move]
solve code = loop [] $ allCodes $ length code
    where
      loop acc [] = acc
      loop acc (c:codes) =
          loop (acc ++ [move]) $ filterCodes move codes
              where move = getMove code c

solve' :: Code -> [Move]
solve' code = loop $ allCodes $ length code
    where
      loop (c:codes) = move : (loop $ filterCodes move codes)
          where move = getMove code c
      loop [] = []
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

data FailableDouble a = Failure
                      | OK a
  deriving Show
