{-# OPTIONS_GHC -Wall #-}
module Lc01List where
i :: Int                   -- цілі представлені в машині 
i = -78                     
n1 :: Integer               -- цілі довільної довжини    
n1 = 2^(2^(2^(2^(2::Int)::Int)::Int)::Int)
d :: Double                -- дійсні з плаваючою крапкою
d = 4.5387
b :: Bool                  -- логічні 
b = True                   --  True і False - конструктори
c :: Char                  -- символи 
c = 'x'  
s :: String                -- рядок – список символів
s = "Hello, world" 
sum0 :: Int -> Int -> Int   -- функція з двома аргументами
sum0  x y = x+y

sum1 :: Integer -> Integer
sum1 n = if n==0 then 0 else n + sum1 (n-1)

sum2 :: Integer -> Integer
sum2 0 = 0
sum2 n = n + sum2 (n-1)

il :: [Int]
il = [3, 1, 4, 1, 200]
cl1, cl2 :: [Char]
cl1 = ['a', 'b', 'c']   
cl2 = 'a' : ('b' : ('c' : []))

lengthl :: [a] -> Int
lengthl xs = if null xs then 0 else 1 + lengthl (tail xs)

suml :: [Int] -> Int
suml xs = if null xs then 0 else head xs + suml (tail xs)  

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = if null xs then [] else                    
                   (f (head xs)) : (mapl f (tail xs))
filterl :: (a -> Bool) -> [a] -> [a]
filterl p xs = if null xs then [] else
           if p (head xs) then (head xs) : filterl p (tail xs) 
                               else  filterl p (tail xs)                        

l1, l2, l3 :: [Int] 
l1 = [1 .. 100] 
l2 = [1, 3 .. 99]   -- l2 = [1, 3 .. 100]
l3 = [10, 9 .. 1]

l4, l5, l6, l7, l8, l9 :: [Int] 
l4 = [x*x | x <- [1 .. 10]]             -- l4 = [1, 4, 9, .., 100]
l5 = [x*x | x <- [1 .. 10], even x]     -- l5 = [4, 16, .., 100]
l6 = [x+y | x <- [1..3], y <- [10,12]]  -- l6 = [11,13,12,14,13,15]
l7 = [x+y | y <- [10,12], x <- [1..3]]  -- l7 = [11,12,13,13,14,15]
l8 = [v | x <- [1..3], y <- [10,12], let v = x*x + y*y] 
l9 = [x+y | x <- [1..3], y <- [x..3]]  -- l9 = [2,3,4,4,5,6]

ones :: [Int]
ones = 1 : ones           -- рекурсія
-- ones = [1,1 ..]         -- арифметична послідовність
numbersFrom :: Int -> [Int]
numbersFrom n = [n, n+1 ..]
-- numbersFrom n = n : numbersFrom (n+1)

primes :: [Int]
primes = 2 : 
    [x | x<- [3,5..], ([y | y <- [1..x], mod x y == 0] ==[1,x])]

sumList1 :: [Integer] -> Integer
sumList1 lst =
  if null lst then 0
    else let x  = head lst 
             xs = tail lst 
           in (if even x then 3+x else 0) + (sumList1 xs)
           
sumList2 :: [Integer] -> Integer
sumList2 = sum . (map (3+)) . (filter even)

           


