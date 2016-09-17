{-# OPTIONS_GHC -Wall #-}

module Tkach01 where
import Data.List
-- ������ 1 -----------------------------------------

listSum :: Num a => [a] -> [a] -> [a]
listSum [] (y:ys) = 0 + y : listSum [] ys

listSum (x:xs) [] = x + 0 : listSum xs []
listSum (x:xs) (y:ys) = x + y : listSum xs ys
listSum [] [] = []




-- ������ 2 ----------------------------------------- 

oddEven :: [a] -> [a]
oddEven  =undefined

-- ������ 3 -----------------------------------------

position :: Eq a => a -> [a] -> Int
position =   undefined


-- ������ 4 -----------------------------------------

frequency :: Eq a => [a] -> [(a,Int)]
frequency = undefined
                     
-- ������ 5 -----------------------------------------

set :: Eq a => [a] -> [a]
set xs =  nub xs 
set [] = [] 

-- ������ 6 -----------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union = undefined

-- ������ 7 -----------------------------------------

intersection :: Eq a => [a] -> [a] -> [a]
intersection = undefined
