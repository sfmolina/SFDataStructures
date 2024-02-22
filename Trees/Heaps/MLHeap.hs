------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  05o23                                            --
--   Copyright 2024 Serafín (@sfmolina)                         --
------------------------------------------------------------------
--[ Implementing a Leftist Biased Heap


module SFDataStructures.Trees.Heaps.MLHeap (
    MLHeap,
    empty,
    singleton,
    insert,
    delMin,
    weight,
    isEmpty,
    minim,
    --heapSort (WIP),
    --fromList (WIP),
    toList
) where

import Data.Maybe (fromJust)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


type Weight = Int
data MLHeap a = Empty | Node a Weight (MLHeap a) (MLHeap a)
    deriving (Show)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MLHeap a
empty = Empty

--

singleton :: a -> MLHeap a
singleton elem = Node elem 1 Empty Empty

--

node :: a -> MLHeap a -> MLHeap a -> MLHeap a
node elem h h'
    | wh > wh'  = Node elem nw h h'
    | otherwise = Node elem nw h' h
    where
        wh  = weight h
        wh' = weight h'
        nw  = 1 + wh + wh'

--

insert :: (Ord a) => a -> MLHeap a -> MLHeap a
insert elem = merge (singleton elem)

--

-- WIP: fromList :: (Ord a) => [a] -> MLHeap a


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


merge :: (Ord a) => MLHeap a -> MLHeap a -> MLHeap a
merge Empty h' = h'
merge h Empty  = h
merge h@(Node e w lt rt) h'@(Node e' w' lt' rt')
    | e <= e'   = node e lt (merge rt h')
    | otherwise = node e' lt' (merge rt' h)

--

delMin :: (Ord a) => MLHeap a -> MLHeap a
delMin (Node _ _ lt rt) = merge lt rt


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


weight :: MLHeap a -> Int
weight Empty = 0
weight (Node _ w _ _) = w

--

isEmpty :: MLHeap a -> Bool
isEmpty Empty   = True
isEmpty _       = False

--

minim :: MLHeap a -> Maybe a
minim Empty           = Nothing
minim (Node e _ _ _)  = Just e

--

toList :: (Ord a) => MLHeap a -> [a]
toList Empty = []
toList h     = fromJust (minim h) : toList (delMin h)

--

-- WIP: heapSort :: (Ord a) => [a] -> [a]
--      heapSort = toList . fromList


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------

-- WIP

---------------------------------------------------------------------------
