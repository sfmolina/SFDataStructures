------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23s23                                            --
--   Copyright 2024 Serafín (@sfmolina)                         --
------------------------------------------------------------------
--[ Implementing a basic linear stack with nodes


module SFDataStructures.Stacks.MNStack (
    MStack,
    empty,
    singleton,
    push,
    pop,
    isEmpty,
    top,
    size,
    fromList,
    toList

) where

import Data.List ( intercalate )


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


type Size = Int
data MStack a = Empty | Node a Size (MStack a)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MStack a
empty = Empty

--

singleton :: a -> MStack a
singleton x = Node x 1 Empty

--

push :: a -> MStack a -> MStack a
push x Empty = singleton x
push x child = Node x (size child + 1) child

--

fromList :: [a] -> MStack a
fromList = foldr push Empty


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


pop :: MStack a -> MStack a
pop Empty            = error "ERROR: pop on empty stack"
pop (Node _ _ child) = child


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


isEmpty :: MStack a -> Bool
isEmpty Empty = True
isEmpty _     = False

--

top :: MStack a -> a
top Empty        = error "ERROR: top on empty stack"
top (Node e _ _) = e

--

size :: MStack a -> Size
size Empty = 0
size (Node _ s _) = s

--

toList :: MStack a -> [a]
toList Empty = []
toList stack@(Node e _ child) = e : toList child


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------


instance (Eq a) => Eq (MStack a) where
  (==) :: (Eq a) => MStack a -> MStack a -> Bool
  Empty        == Empty          =  True
  (Node e _ c) == (Node e' _ c') =  e==e' && c==c'
  _            == _              =  False

--

instance (Show a) => Show (MStack a) where
  show :: (Show a) => MStack a -> String
  show s = "MStack( " ++ intercalate " | " (aux s) ++ " )"
    where
     aux Empty        =  []
     aux (Node e _ s) =  show e : aux s


---------------------------------------------------------------------------
