{-# OPTIONS_GHC -Wall #-}

module Tkach01 where
import Data.List
-- Задача 1 -----------------------------------------

listSum :: Num a => [a] -> [a] -> [a]
listSum [] (y:ys) = 0 + y : listSum [] ys

listSum (x:xs) [] = x + 0 : listSum xs []
listSum (x:xs) (y:ys) = x + y : listSum xs ys
listSum [] [] = []




-- Задача 2 ----------------------------------------- 

oddEven :: [a] -> [a]
oddEven  =undefined

-- Задача 3 -----------------------------------------

position :: Eq a => a -> [a] -> Int
position =   undefined


-- Задача 4 -----------------------------------------

frequency :: Eq a => [a] -> [(a,Int)]
frequency = undefined
                     
-- Задача 5 -----------------------------------------

set :: Eq a => [a] -> [a]
set xs =  nub xs 
set [] = [] 

-- Задача 6 -----------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union = undefined

-- Задача 7 -----------------------------------------

intersection :: Eq a => [a] -> [a] -> [a]
intersection = undefined
