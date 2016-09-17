{-# OPTIONS_GHC -Wall #-}

module Tkach01 where
import  qualified Data.List as List
import qualified Data.Set as Set
-- Задача 1 -----------------------------------------

listSum :: Num a => [a] -> [a] -> [a]
listSum [] (y:ys) = 0 + y : listSum [] ys

listSum (x:xs) [] = x + 0 : listSum xs []
listSum (x:xs) (y:ys) = x + y : listSum xs ys
listSum [] [] = []




-- Задача 2 ----------------------------------------- 

oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven (x:(y:ys)) = [y,x]++oddEven ys

-- Задача 3 -----------------------------------------

position :: Eq a => a -> [a] -> Int
position _ [] = 0
position elem [] = 0
position elem (x:xs) | x == elem = 1
				       | x /= elem = 1 + nextPosition 1 (length xs) (elem) xs
				  

-- Задача 4 -----------------------------------------


frequency :: Eq a => [a] -> [(a, Int)]
frequency [] = []
frequency xl = [(elem, count elem xl) | elem <- (List.nub xl)]


                     
-- Задача 5 -----------------------------------------

set :: Eq a => [a] -> [a]
set xs =  List.nub xs 
set [] = [] 

-- Задача 6 -----------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union [] [] = []
union [] ys = List.nub ys
union  xs [] = List.nub xs
union xs ys = List.nub (xs ++ ys)

-- Задача 7 -----------------------------------------

intersection :: Eq a => [a] -> [a] -> [a]
intersection xl yl = [el | el <- xl, el`elem` yl]
----------- SPECIAL  HELPER FUNCTION ------

nextPosition :: Eq a => Int -> Int -> a -> [a] -> Int
nextPosition _ _ _ [] = 0
nextPosition iteration arrayLength needle (x:xs) | x == needle = 1
					                             | iteration == arrayLength = -(arrayLength)
				                                 | x /= needle = 1 + nextPosition (iteration + 1) arrayLength needle xs
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) | n == x = 1 + count n xs
               | otherwise = count n xs



