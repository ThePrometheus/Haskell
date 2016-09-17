{-# OPTIONS_GHC -Wall #-}
module Lc02Funct where
simple :: Int -> Int -> Int -> Int
simple x y z = x+y+z

-- останній елемент списку
last0 :: [a] -> a     
last0  [x]    = x 
last0  (_:xs) = last0 xs
last0  []     = error "Empty  list !"

headDup :: [a] -> [a]
headDup []      = []
headDup l@(x:_) = x:l

add1 :: [Int] -> [Int]  -- додає 1 до всіх елементів
add1 = map (\ x -> x+1)
           
length1 :: [a] -> Int
length1 []       = 0 
length1 (_:xs) = 1+ length1 xs

sum1 :: [Int] -> Int
sum1  []      = 0
sum1 (x:xs) = x + sum1 xs  

map1 :: (a -> b) -> [a] -> [b]
map1 _ []      = []                     
map1 f (x:xs) =  f x : map1 f  xs

max1 :: Int -> Int -> Int
max1  x y | x > y = x
          | otherwise = y

compBeg :: [Int] -> Char
compBeg (x : (y : _)) | x > y = 'G'
                      | x < y = 'L'
compBeg  _ = 'N'
keepOnlyPos :: [Int] -> [Int]
keepOnlyPos [] = []
keepOnlyPos (x:xs) 
         | x>0        = x : keepOnlyPos xs
         |otherwise = keepOnlyPos xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ []     = []
filter1 p (x:xs) 
    | p x         = x : filter1 p xs
    |otherwise = filter1 p xs

keepOnlyPos1 :: [Int] -> [Int]
keepOnlyPos1 = filter (>0)   

sum2, sum3 :: [Int] -> Int   -- sm - рекурсія і акумулятор
sum2 ys =
    let sm [] total       = total
        sm (x:xs) total = sm xs (total+x)
    in sm ys 0  

sum3 ys = sm ys 0
     where sm [] total = total
           sm (x:xs) total = sm xs (total + x)

last1, last2 :: [a] -> a
last1 l  = case l of
               [x]    -> x
               (_:xs) -> last1 xs 
               []     -> error "Empty list"

last2 [x]    = x
last2 (_:xs) = last2 xs
last2 []     = error "Empty list"
