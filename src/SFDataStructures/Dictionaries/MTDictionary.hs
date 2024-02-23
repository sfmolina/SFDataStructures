------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
------------------------------------------------------------------
--[ Implementing a basic dictionary with an AVL tree


module SFDataStructures.Dictionaries.MTDictionary (
    MTDict,
    empty,
    isEmpty,
    insert,
    valueOf,
    fromListPairs,
    toListPairs
) where

import qualified SFDataStructures.Trees.BST.MAVL as AVL
import Data.List ( intercalate )


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- RELATIONS --------------------------------------------------------------


data Rel a b = a :->: b

--

instance (Eq a) => Eq (Rel a b) where
    (k :->: _) == (k' :->: _) = k == k'

--

instance (Ord a) => Ord (Rel a b) where
    (k :->: _) <= (k' :->: _) = k <= k'


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


newtype MTDict a b = D (AVL.MAVL (Rel a b))


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


-- | Returns an empty dictionary.
-- - O(1)
empty :: MTDict a b
empty = D AVL.empty

--

-- | Inserts a new key-value pair into the dictionary.
-- - O(log n)
insert :: (Ord a) => a -> b -> MTDict a b -> MTDict a b
insert k v (D tree) = D (AVL.insert (k :->: v) tree)

--

-- | Converts a list of key-value pairs into a dictionary.
--   The keys must be orderable.
--   The resulting dictionary is created by folding over the list and inserting each pair.
-- - O(n log n)
fromListPairs :: (Ord a) => [(a, b)] -> MTDict a b
fromListPairs = foldr insertPair empty
    where
        -- | Inserts a key-value pair into a multi-threaded dictionary.
        --   If the key already exists, the value is updated.
        insertPair :: (Ord a) => (a, b) -> MTDict a b -> MTDict a b
        insertPair (k, v) = insert k v


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


-- | Returns Just the value associated with a given key or Nothing if the key is not in the dictionary.
-- - O(log n)
valueOf :: (Ord a) => a -> MTDict a b -> Maybe b
valueOf k (D tree) = case AVL.search (k :->: undefined) tree of
    Nothing        -> Nothing
    Just (_ :->: v) -> Just v

--

-- | Convert a dictionary to a list of key-value pairs.
--   The resulting list is ordered by the keys in ascending order.
-- - O(n)
toListPairs :: MTDict a b -> [(a, b)]
toListPairs (D tree) = map (\(k :->: v) -> (k, v)) (AVL.toListInOrder tree)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


-- | Returns if the dictionary is empty.
-- - O(1)
isEmpty :: MTDict a b -> Bool
isEmpty (D tree) = AVL.isEmpty tree


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------


instance (Show a, Show b) => Show (MTDict a b) where
    show dict
        | isEmpty dict = "MDictionary{}"
        | otherwise = "MDictionary{\n" ++ intercalate "\n" (aux pairs) ++ "\n}"
        where
            pairs = toListPairs dict

            aux :: (Show a, Show b) => [(a, b)] -> [String]
            aux [] = []
            aux ((k, v):xs) = ("  " ++ show k ++ " --> " ++ show v) : aux xs

--

instance (Eq a, Eq b) => Eq (MTDict a b) where
    dict == dict' = toListPairs dict == toListPairs dict'


---------------------------------------------------------------------------
