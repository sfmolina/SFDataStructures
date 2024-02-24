------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
------------------------------------------------------------------
--[ Implementing a basic Binary Seacrh Tree


module SFDataStructures.Trees.BST.MBST (
    MBST,
    empty,
    insert,
    delete,
    isEmpty,
    height,
    search,
    isElem,
    minim,
    maxim,
    fromList,
    toListInOrder
) where

import qualified SFDataStructures.Trees.BST.InterBST as Inter


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


type MBST a = Inter.MBST a

function :: a -> MBST a -> MBST a -> MBST a
function = Inter.node -- This function is what identifies a basic Binary Search Tree


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MBST a
empty = Inter.empty

--

insert :: (Ord a) => a -> MBST a -> MBST a
insert = Inter.insert function

--

fromList :: (Ord a) => [a] -> MBST a
fromList = Inter.fromList function


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


delete :: (Ord a) => a -> MBST a -> MBST a
delete = Inter.delete function

--

toListInOrder :: MBST a -> [a]
toListInOrder = Inter.toListInOrder


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


height :: MBST a -> Int
height = Inter.height

--

isEmpty :: MBST a -> Bool
isEmpty = Inter.isEmpty

--

search :: (Ord a) => a -> MBST a -> Maybe a
search = Inter.search

--

isElem :: (Ord a) => a -> MBST a -> Bool
isElem = Inter.isElem

--

minim :: MBST a -> Maybe a
minim = Inter.minim

--

maxim :: MBST a -> Maybe a
maxim = Inter.maxim


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------

-- WIP

---------------------------------------------------------------------------
