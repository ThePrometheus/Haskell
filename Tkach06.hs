{-# OPTIONS_GHC -Wall #-}
module Tkach06 where

type Graph = [[Int]]



-- Çàäà÷à 1 -----------------------------------------  
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr st end = 
    let arr = dfs1 gr st end []
    in if (null arr) then Nothing else Just (snd $ maximum $ map (\x -> (length x, x)) (arr))

-- Çàäà÷à 2 -----------------------------------------  

isNoCycle :: Graph -> Bool
isNoCycle gr = foldl (&&) True [(dfs2 gr x []) | x <- [1 .. (length gr)]]
   
-- Çàäà÷à 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr = foldl (\r1 a -> r1 && foldl (\r2 b -> r2 && foldl (\r3 c -> r3 && (c `elem` (gr !! (a-1)))) True (gr !! (b-1))) True (gr !! (a-1))) True [1..(length gr)]

-- Çàäà÷à 4 -----------------------------------------
isGraph :: Graph -> Bool
isGraph gr = foldl (\a b -> a && ( foldl (\x y -> x && (y > 0) && (y <= (length gr)) && (b `elem` (gr !! (y - 1))) ) True (gr !! (b-1)) ) ) True [1..(length gr)]

-- Çàäà÷à 5 -----------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay  gr st end = 
    let arr = dfs1 gr st end []
    in if (null arr) then Nothing else Just (snd $ minimum $ map (\x -> (length x, x)) (arr))

-- Çàäà÷à 6 -----------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = (length $ components gr) == 1

-- Çàäà÷à 7 -----------------------------------------

unique :: [Int] -> [Int]
unique li = foldl (\a b -> if (not (b `elem` a)) then b:a else a) [] li



isNew :: Int -> [[Int]] -> Bool
isNew _ [] = True
isNew el li = foldl (\a b -> a && not ( (el `elem` b))) True li

components :: Graph -> [[Int]]
components gr = if (isGraph gr) then foldl (\a b -> if (isNew b a) then (unique $ (dfs3 gr b [])):a else a) [] [1 .. (length gr)] else []
---------HELPER FUNCTIONS----------------
dfs1 :: Graph -> Int -> Int -> [Int] -> [[Int]]
dfs1 gr st end t
                | (st == end) = [[st]]
                | (st `elem` t) = []
                | otherwise = foldl (\a b -> map (\c -> st : c) (dfs1 gr b end (st:t)) ++ a) [] (gr !! (st-1))
dfs2 :: Graph -> Int -> [Int] -> Bool
dfs2 gr st t = (not (st `elem` t)) && (foldl (\a b -> (dfs2 gr b (st:t)) && a) True (gr !! (st-1)))
dfs3 :: Graph -> Int -> [Int] -> [Int]
dfs3 gr st t
            | (st `elem` t) = []
            | otherwise =  st: foldl (\a b -> dfs3 gr b (st:t) ++ a) [] (gr !! (st-1))
 