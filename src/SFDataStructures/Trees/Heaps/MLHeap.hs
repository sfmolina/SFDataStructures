------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
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
    heapSort,
    fromList,
    toList
) where

import Data.Maybe (fromJust)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


type Weight = Int

-- | A MLHeap is a leftist biased heap.
--   It is a complete binary tree with the following properties:
--   - The weight of the left child is greater than or equal to the weight of the right child.
--   - The weight of a node is the number of elements in the subtree rooted at that node.
--   - The top node is the minimum element in the heap. And every node is greater than or equal to its father (HOP, Heap Order Property).
data MLHeap a = Empty | Node a Weight (MLHeap a) (MLHeap a)
    deriving (Show)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


-- | Creates an empty MLHeap.
-- - O(1)
empty :: MLHeap a
empty = Empty

--

-- | Creates a singleton MLHeap with the given element.
-- - O(1)
singleton :: a -> MLHeap a
singleton element = Node element 1 Empty Empty

--

-- | Creates a new node in the MLHeap data structure.
-- 
-- The node function takes an element of type 'a' and two MLHeap trees 'h' and 'h''.
-- It returns a new MLHeap tree with the given element as the root and the two input trees as its children.
-- The children are arranged such that the weight of the left child is greater than the weight of the right child.
-- - O(1)
node :: a -> MLHeap a -> MLHeap a -> MLHeap a
node element h h'
    | wh > wh'  = Node element nw h h'
    | otherwise = Node element nw h' h
    where
        wh  = weight h
        wh' = weight h'
        nw  = 1 + wh + wh'

--

-- | Insert an element into the MLHeap.
--
-- Takes an element of type 'a' and an MLHeap, and returns a new MLHeap with the element inserted.
-- The element is inserted at the root of the heap, and the heap property is maintained.
--
-- - O(log n)
insert :: (Ord a) => a -> MLHeap a -> MLHeap a
insert element = merge (singleton element)

--

-- | Converts a list of elements into a MLHeap.
--   The elements are first converted into singleton MLHeaps,
--   and then divided and merged recursively until a single MLHeap is obtained.
-- - O(n log n)
fromList :: (Ord a) => [a] -> MLHeap a
fromList = divideAndMerge . map singleton
    where
        -- | Recursively divides and merges a list of MLHeaps until a single MLHeap is obtained.
        divideAndMerge :: (Ord a) => [MLHeap a] -> MLHeap a
        divideAndMerge [] = empty
        divideAndMerge [h] = h
        divideAndMerge list = merge (divideAndMerge lhalf) (divideAndMerge rhalf)
            where
                (lhalf, rhalf) = splitAt (length list `div` 2) list


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


-- | Merges two 'MLHeap's into a single 'MLHeap'.
--   It does that by merging the two heaps using their right spines.
-- - O(log n)
merge :: (Ord a) => MLHeap a -> MLHeap a -> MLHeap a
merge Empty h' = h'
merge h Empty  = h
merge h@(Node e _ lt rt) h'@(Node e' _ lt' rt')
    | e <= e'   = node e lt (merge rt h')
    | otherwise = node e' lt' (merge rt' h)

--

-- | Deletes the minimum element from the 'MLHeap'.
--   Returns a new MLHeap without the minimum element.
-- - O(log n)
delMin :: (Ord a) => MLHeap a -> MLHeap a
delMin Empty = Empty
delMin (Node _ _ lt rt) = merge lt rt


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


-- | Calculates the weight of an 'MLHeap'.
--   The weight of an 'MLHeap' is defined as the number of elements in the heap.
-- - O(1)
weight :: MLHeap a -> Int
weight Empty = 0
weight (Node _ w _ _) = w

--

-- |Check if the MLHeap is empty.
-- - O(1)
isEmpty :: MLHeap a -> Bool
isEmpty Empty   = True
isEmpty _       = False

--


-- | Returns the minimum element of the 'MLHeap', if it is not empty.
--   If the MLHeap is empty, returns 'Nothing'.
-- - O(1)
minim :: MLHeap a -> Maybe a
minim Empty           = Nothing
minim (Node e _ _ _)  = Just e

--

-- | Convert an MLHeap to a list.
--   The elements are extracted in ascending order.
-- - O(n)
toList :: (Ord a) => MLHeap a -> [a]
toList Empty = []
toList h     = fromJust (minim h) : toList (delMin h)

--

-- | Sorts a list using the heap sort algorithm.
--   The elements in the list must be orderable.
--   Returns a new list with the elements sorted in ascending order.
-- - O(n log n)
heapSort :: (Ord a) => [a] -> [a]
heapSort = toList . fromList


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------

-- WIP

---------------------------------------------------------------------------
