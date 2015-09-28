{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l) == (P r) = trimmedL == trimmedR
        where trimmedL = (take longerLength (dropWhileEnd (==0) l))
              trimmedR = (take longerLength (dropWhileEnd (==0) r))
              longerLength = max (length l) (length r)
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P cs) = intercalate " + " $ reverse $ filter ((>0).length) $ showPoly cs 0
        where
            showPoly (c:cs) p = showTerm c p : showPoly cs (p+1)
            showPoly [] _ = []
            showTerm 0 _ = ""
            showTerm c 0 = (show c)
            showTerm (-1) 1 = "-x"
            showTerm 1 1 = "x"
            showTerm c 1 = (show c) ++ "x"
            showTerm 1 p = "x^" ++ (show p)
            showTerm c p = (show c) ++ "x^" ++ (show p)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l) (P r) = P $ zipWith (+) (pad l) (pad r)
    where pad xs = xs ++ (take (longer - (length xs)) $ replicate 0)
          longer = max (length l) (length r)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) r = r
times (P l) (P r) = foldr (+) (P [0]) (times' l r [])
    where times' (l:ls) r zs = (P $ (zs ++ map (*l) r)) : (times' ls r (0:zs))
          times' [] _ _ = [P []]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P ps) = P $ map (*(-1)) ps
    fromInteger x = P [fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P []) _ = 0
applyP (P (c:cs)) x = c + x * applyP (P cs) x

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 p = p
    nderiv n p = nderiv (n-1) . deriv $ p

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P [0]
    deriv (P (_:cs)) = P $ deriv' cs
        where deriv' [] = [0]
              deriv' (x:xs) = zipWith (+) (x:xs) (0 : deriv' xs)
