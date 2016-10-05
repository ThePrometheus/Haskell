{-# OPTIONS_GHC -Wall #-}
module Tkach04 where

data OrdTree a = OrdTree a [OrdTree a]  
                   deriving (Show)	
data BinTree a = Empty 
                 | Node a (BinTree a) (BinTree a)
			       deriving (Show, Eq)



t = OrdTree 5 [OrdTree 3 [OrdTree 1 [], OrdTree 4[]], OrdTree 7 []] 
h = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))

-- Çàäà÷à 1 -----------------------------------------			   
sumTree     :: (Num a) =>  OrdTree a -> a
sumTree (OrdTree a b) = a + sum(fmap sumTree b)
-- Çàäà÷à 2-----------------------------------------
dfsTreeList ::  OrdTree a->  [a]
dfsTreeList (OrdTree a b ) = a : concat[dfsTreeList n|n <- b]

-- Çàäà÷à 3 -----------------------------------------
bfsTreeList ::  OrdTree a ->  [a]
bfsTreeList a = bfs [a] 
    where 
	bfs [] = []
	bfs tree = (map getRootOfTree tree) ++ bfs (concat (map getListOfTrees tree))

-- Çàäà÷à 4 -----------------------------------------
equalTree   :: (Eq a) => OrdTree a -> OrdTree a -> Bool
equalTree (OrdTree n1 xs1) (OrdTree n2 xs2) = n1 == n2 && listTreeEquals xs1 xs2
    where
    listTreeEquals [] [] = True
    listTreeEquals (x1 : xs1) (x2 : xs2) = equalTree x1 x2 && listTreeEquals xs1 xs2
    listTreeEquals _ _ = False
treeEquals _ _ = False
 
-- Çàäà÷à 5 -----------------------------------------
isInTree    :: (Eq a) => OrdTree a -> OrdTree a -> Bool
isInTree a b 
    |equalTree a b = True
    |True `elem` (map (isInTree a) (getListOfTrees b)) = True
    |otherwise = False


-- Çàäà÷à 6-----------------------------------------
toBinTree   :: [OrdTree a] ->  BinTree a
toBinTree [] = Empty
toBinTree (x:xs) = Node (getRootOfTree x) (toBinTree (getListOfTrees x)) (toBinTree xs)

-- Çàäà÷à 7 -----------------------------------------  
toOrdTree   ::  BinTree a -> [OrdTree a] 
toOrdTree Empty = []
toOrdTree (Node a left right) = OrdTree a (toOrdTree (left)) : (toOrdTree (right))

getListOfTrees (OrdTree a []) = []
getListOfTrees (OrdTree _ b) = b
getRootOfTree (OrdTree a _) = a