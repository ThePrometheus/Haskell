{-# OPTIONS_GHC -Wall #-}
module Tkach05 where

data BinTree a = Empty 
                 | Node a (BinTree a) (BinTree a)
                 deriving (Show, Eq)
data Btree a = 
       NodeB [a] [Btree a]  
       deriving (Show, Eq)

h = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))
isSearch :: (Ord a) => BinTree a -> Bool
isSearch Empty = True

isSearch (Node a left right)
    |(isEmpty(left)) && (isEmpty(right)) = True
    |(isEmpty(left)) && (a>getNode right) = False
    |(isEmpty(left)) && (a<getNode right) =isSearch(right)
    |(isEmpty(right)) && (a<getNode left) = False
    |(isEmpty(right)) && (a>getNode left) = isSearch(left)
    |a<getNode left = False
    |a>getNode right = False
    |a>getNode left = isSearch(left)
    |a<getNode right = isSearch(right)

-- Çàäà÷à 2-----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch Empty x = singleton x
insSearch (Node _ _ _) _ = error "Nothing inserted"
insSearch (Node a left right) x
    |x==a = Node x left right
    |x<a = Node a (insSearch left x) right
    |x>a = Node a left (insSearch right x)

-- Çàäà÷à 3 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch Empty _ = error "No instance"
delSearch (Node a left right) x 
    | x < a     = Node a (delSearch left x) right
    | x > a     = Node a left (delSearch right x)
    | otherwise = keepBalanced left right
    where  keepBalanced :: (Ord a) => BinTree a -> BinTree a -> BinTree a
           keepBalanced Empty right = right
           keepBalanced left Empty = left
           keepBalanced left right = Node a' left' right
               where a' = findRightMost left
                     left' = delSearch left a' 
                     
                     findRightMost :: (Ord a) => BinTree a -> a
                     findRightMost Empty = error "No right most value found"
                     findRightMost (Node rm _ Empty) = rm
                     findRightMost (Node _  _ right)   = findRightMost right


-- Çàäà÷à 4 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]	
sortList [] = []
sortList xs =  sortListB(listToTree xs)


sortListB       :: BinTree a -> [a]
sortListB Empty = []
sortListB (Node a left right) = sortListB left ++ [a] ++ sortListB right
 
-- Çàäà÷à 5 -----------------------------------------
isBtree  :: Ord a => Int -> Btree a -> Bool 
isBtree  = undefined

-- Çàäà÷à 6-----------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree  = undefined

getNode::BinTree a -> a
getNode (Node a _ _) = a
getNode Empty = error "no node"

isEmpty :: (Ord a) => BinTree a -> Bool
isEmpty Empty = True
isEmpty _   = False

listToTree :: (Ord a) => [a] -> BinTree a
listToTree xs = foldl insSearch Empty xs

singleton :: a -> BinTree a
singleton x = Node x Empty Empty




			   



  