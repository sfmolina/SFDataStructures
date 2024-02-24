------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
------------------------------------------------------------------
--[ Implementing a Balanced Binary Search Tree. It is identical to a BST but with a condition of balance


module SFDataStructures.Trees.BST.MAVL (
    MAVL,
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


type MAVL a = Inter.MBST a

function :: a -> MAVL a -> MAVL a -> MAVL a
function = balance -- This function is what identifies a Balanced Binary Search Tree


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MAVL a
empty = Inter.empty

--

insert :: (Ord a) => a -> MAVL a -> MAVL a
insert = Inter.insert function

--

fromList :: (Ord a) => [a] -> MAVL a
fromList = Inter.fromList function


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


delete :: (Ord a) => a -> MAVL a -> MAVL a
delete = Inter.delete function

--

toListInOrder :: MAVL a -> [a]
toListInOrder = Inter.toListInOrder


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- ROTATION AND BALANCE ---------------------------------------------------


rightLeaning :: MAVL a -> Bool
rightLeaning (Inter.Node _ _ lt rt) = height lt <= height rt

--

leftLeaning :: MAVL a -> Bool
leftLeaning (Inter.Node _ _ lt rt) = height lt >= height rt

--

rotR :: MAVL a -> MAVL a
rotR (Inter.Node e _ (Inter.Node elt _ ltlt rtlt) rt) = Inter.node elt ltlt (Inter.node e rtlt rt)

--

rotL :: MAVL a -> MAVL a
rotL (Inter.Node e _ lt (Inter.Node ert _ ltrt rtrt)) = Inter.node ert (Inter.node e lt ltrt) rtrt

--

balance :: a -> MAVL a -> MAVL a -> MAVL a
balance e lt rt
    | (hlt - hrt > 1) && leftLeaning lt     = rotR (Inter.node e lt rt)
    | (hlt - hrt > 1)                       = rotR (Inter.node e (rotL lt) rt)
    | (hrt - hlt > 1) && rightLeaning rt    = rotL (Inter.node e lt rt)
    | (hrt - hlt > 1)                       = rotL (Inter.node e lt (rotR rt))
    | otherwise                             = Inter.node e lt rt
    where
        hrt = height rt 
        hlt = height lt


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


height :: MAVL a -> Int
height = Inter.height

--

isEmpty :: MAVL a -> Bool
isEmpty = Inter.isEmpty

--

search :: (Ord a) => a -> MAVL a -> Maybe a
search = Inter.search

--

isElem :: (Ord a) => a -> MAVL a -> Bool
isElem = Inter.isElem

--

minim :: MAVL a -> Maybe a
minim = Inter.minim

--

maxim :: MAVL a -> Maybe a
maxim = Inter.maxim


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------

-- WIP

---------------------------------------------------------------------------
