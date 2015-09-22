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
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length . filter (==True) $ zipWith (==) c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors cs = map (countColor cs) colors
    where countColor cs c = length $ filter (== c) cs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches cs1 cs2 = sum $ zipWith min (countColors cs1) (countColors cs2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess exacts nonexacts
    where exacts = exactMatches actual guess
          nonexacts = matches actual guess - exacts

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move mc _ _) c = m == getMove c mc

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (:[]) colors
allCodes x = concatMap (combine colors) $ allCodes (x-1)
    where combine colors c = map (:c) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = solve' code $ allCodes (length code)

solve' :: Code -> [Code] -> [Move]
solve' _ [] = []
solve' c (g:gs) = currentMove : solve' c remainingCodes
    where currentMove    = getMove c g
          remainingCodes = filterCodes currentMove gs

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
