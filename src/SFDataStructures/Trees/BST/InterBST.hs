------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
------------------------------------------------------------------
--[ This is a module with functions shared between MBST
--[ and MAVL that each one uses diferently


module SFDataStructures.Trees.BST.InterBST (
    MBST (Empty, Node),
    empty,
    node,
    insert,
    delete,
    height,
    isEmpty,
    search,
    isElem,
    minim,
    maxim,
    fromList,
    toListInOrder
) where

import Data.Maybe (isJust)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


type Height = Int
data MBST a = Empty | Node a Height (MBST a) (MBST a)
    deriving (Show)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MBST a
empty = Empty

--

node :: a -> MBST a -> MBST a -> MBST a
node x lt rt = Node x h lt rt
    where
        h = 1 + max (height lt) (height rt)

--

--        (Ord a) => func                              -> a -> MBST a -> MBST a
insert :: (Ord a) => (a -> MBST a -> MBST a -> MBST a) -> a -> MBST a -> MBST a
insert _    x   Empty = node x Empty Empty
insert func x n@(Node e _ lt rt)
    | x == e    = n
    | x <  e    = func e lt' rt
    | otherwise = func e lt rt'
    where
        lt' = insert func x lt
        rt' = insert func x rt

--

fromList :: (Ord a) => (a -> MBST a -> MBST a -> MBST a) -> [a] -> MBST a
fromList func = foldr (insert func) Empty


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


--        (Ord a) => func                              -> a -> MBST a -> MBST a
delete :: (Ord a) => (a -> MBST a -> MBST a -> MBST a) -> a -> MBST a -> MBST a
delete _    _ Empty = Empty
delete func x (Node e _ lt rt)
    | x == e    = combine func lt rt
    | x <  e    = func e lt' rt
    | otherwise = func e lt  rt'
    where
        lt' = delete func x lt
        rt' = delete func x rt

--

--         (Ord a) => func                              -> MBST a -> MBST a -> MBST a
combine :: (Ord a) => (a -> MBST a -> MBST a -> MBST a) -> MBST a -> MBST a -> MBST a
combine _ lt Empty  = lt
combine _ Empty rt  = rt
combine func lt rt  = func x' lt rt'
    where
        (x', rt') = split func rt

--

toListInOrder :: MBST a -> [a]
toListInOrder Empty = []
toListInOrder (Node e _ lt rt) = toListInOrder lt ++ [e] ++ toListInOrder rt


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


--[ Erases and returns the minimum element in the bst
--       (Ord a) => func                              -> MBST a -> (a, MBST a)
split :: (Ord a) => (a -> MBST a -> MBST a -> MBST a) -> MBST a -> (a, MBST a)
split _    (Node x _ Empty rt)  = (x, rt)
split func (Node x _ lt rt)     = (x', func x lt' rt)
    where
        (x', lt') = split func lt

--

height :: MBST a -> Int
height Empty          = 0
height (Node _ h _ _) = h

--

isEmpty :: MBST a -> Bool
isEmpty Empty = True
isEmpty _     = False

--

search :: (Ord a) => a -> MBST a -> Maybe a
search _ Empty = Nothing
search x (Node e _ lt rt)
    | x == e    = Just e
    | x <  e    = search x lt
    | otherwise = search x rt

--

isElem :: (Ord a) => a -> MBST a -> Bool
isElem x bst = isJust $ search x bst

--

minim :: MBST a -> Maybe a
minim Empty = Nothing
minim (Node e _ Empty _) = Just e
minim (Node _ _ lt _)   = minim lt

--

maxim :: MBST a -> Maybe a
maxim Empty = Nothing
maxim (Node e _ _ Empty) = Just e
maxim (Node _ _ _ rt)   = maxim rt


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------

-- WIP

---------------------------------------------------------------------------
