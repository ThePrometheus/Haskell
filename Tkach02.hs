{-# OPTIONS_GHC -Wall #-}
module Tkach02 where

-- Задача 1 -----------------------------------------

sumRec :: [Integer] -> Integer
sumRec [] = 0
sumRec (x:xs) = x + sumRec(xs)
  
-- Задача 2 ----------------------------------------- 

sumFl :: [Integer] -> Integer
sumFl []   = 0
sumFl xs = foldl (+) 0 xs

-- Задача 3 -----------------------------------------

productFr :: [Integer] -> Integer
productFr  [] = 0
productFr (x:xs) = foldr (*) x xs
-- Задача 4 -----------------------------------------

concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr [] [] = []
concatFr [] ys = ys
concatFr xs [] = xs
concatFr xs ys = foldr (:) ys xs

-- Задача 5 -----------------------------------------

sortInsert :: [Integer] -> [Integer]
sortInsert  = foldr insert [] 
-- Задача 6 -----------------------------------------

map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []

map2 _ _ [] = []

map2 f (x:xs) (y:ys) = f x y : map2 f xs ys 

-- Задача 7  -----------------------------------------

factorialsM :: [Integer]
factorialsM = 1 : map2 (*) factorialsM [1..5]

-- Задача 8 -----------------------------------------

primes :: [Integer]
primes = 2 : 
    [x | x<- [3,5..], ([y | y <- [1..x], mod x y == 0] ==[1,x])]

insert :: Integer -> [Integer] -> [Integer]
insert x [] =[x]
insert x (y:ys) = if x < y
                   then x:y:ys
                   else y: insert x ys