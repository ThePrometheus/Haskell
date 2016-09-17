{-# OPTIONS_GHC -Wall #-}

module Tkach01 where
import Data.List as List
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
position needle (x:xs) | x == needle = 1
				       | x /= needle = 1 + positionI 1 (length xs) (needle) xs
				  
positionI :: Eq a => Int -> Int -> a -> [a] -> Int
positionI _ _ _ [] = 0
positionI iteration arrayLength needle (x:xs) | x == needle = 1
					                          | iteration == arrayLength = -(arrayLength)
				                              | x /= needle = 1 + positionI (iteration + 1) arrayLength needle xs
-- Задача 4 -----------------------------------------

frequency :: Eq a => [a] -> [(Int, a)]
frequency [] = []
frequency inputList = [(count currentElement inputList, currentElement) | currentElement <- (List.nub inputList)]


                     
-- Задача 5 -----------------------------------------

set :: Eq a => [a] -> [a]
set xs =  List.nub xs 
set [] = [] 

-- Задача 6 -----------------------------------------

unionT :: Eq a => [a] -> [a] -> [a]
unionT [] [] = []
unionT [] ys = List.nub ys
unionT  xs [] = List.nub xs
unionT xs ys = List.nub (xs ++ ys)

-- Задача 7 -----------------------------------------

intersectionT :: Eq a => [a] -> [a] -> [a]
intersectionT xl yl = [el | el <- xl, el`elem` yl]


count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) | n == x = 1 + count n xs
           | otherwise = count n xs
