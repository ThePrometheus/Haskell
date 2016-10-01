{-# OPTIONS_GHC -Wall #-}
module Tkach03 where
-- Задача 1.a -----------------------------------------

evens :: Integer -> [Integer]
evens n = [2,4..2*n]

-- Задача 1.b -----------------------------------------

toPower2 :: Integer -> [Integer]
toPower2 n = [2^x|x<-[0..n]]

-- Задача 1.c -----------------------------------------

triangle :: Integer -> [ Integer]
triangle n = [ div (x * x +  x) 2 | x<-[1..n]]

-- Задача 1.d -----------------------------------------

piramid :: Integer -> [Integer]
piramid n = [div (x*(x+1)*(2*x+1) ) 6 | x<- [1..n]]

-- Задача 2.a -----------------------------------------

power5 :: [Integer]
power5 = [x^5 | x<-[1..]]

-- Задача 2.b -----------------------------------------

toPower5 :: [Integer]
toPower5  = [5^x | x<-[1..]]

-- Задача 2.c -----------------------------------------

super2 :: [Integer]
super2  = [x^x | x<-[1..]]

-- Задача 2.d -----------------------------------------

super3 :: [Integer]
super3  = [x^(x^x) | x<- [1..]]

-- Задача 3 -----------------------------------------

sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^x | x <- [0..n]]

-- Задача 4.a -----------------------------------------
sumPowerM :: Integer -> Integer -> Integer
sumPowerM  n i = sum [n^x | x<- [0..i]]

-- Задача 4.b -----------------------------------------
expMPart :: Integer -> Integer -> Double
expMPart m n = sum [(fromIntegral m^ (fromIntegral x))/(fromIntegral x) | x <- [1..n]]
