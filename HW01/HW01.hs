{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (flip mod) 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (flip div) 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits i
    | i > 0 = lastDigit i : toRevDigits (dropLastDigit i)
    | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (a:b:is) = a : b*2 : doubleEveryOther is
doubleEveryOther (a:[]) = [a]
doubleEveryOther _ = []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toRevDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (==0) . lastDigit . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi i a b c = hanoi ix a c b ++ hanoi 1 a b c ++ hanoi ix c b a
    where ix = i - 1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c d = hanoi4 1 a c b d ++ hanoi4 1 a b c d ++ hanoi4 1 c b a d
hanoi4 i a b c d = hanoi4 ix a d b c ++ hanoi4 2 a b c d ++ hanoi4 ix d b a c
    where ix = i - 2
