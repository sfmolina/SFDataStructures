------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--   Copyright 2024 Serafín (@sfmolina)                         --
------------------------------------------------------------------
--[ Implementing a basic linear stack with nodes


module SFDataStructures.Stacks.MNStack (
    MNStack,
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
data MNStack a = Empty | Node a Size (MNStack a)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MNStack a
empty = Empty

--

singleton :: a -> MNStack a
singleton x = Node x 1 Empty

--

push :: a -> MNStack a -> MNStack a
push x Empty = singleton x
push x child = Node x (size child + 1) child

--

fromList :: [a] -> MNStack a
fromList = foldr push Empty


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


pop :: MNStack a -> MNStack a
pop Empty            = error "ERROR: pop on empty stack"
pop (Node _ _ child) = child


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


isEmpty :: MNStack a -> Bool
isEmpty Empty = True
isEmpty _     = False

--

top :: MNStack a -> a
top Empty        = error "ERROR: top on empty stack"
top (Node e _ _) = e

--

size :: MNStack a -> Size
size Empty = 0
size (Node _ s _) = s

--

toList :: MNStack a -> [a]
toList Empty = []
toList (Node e _ child) = e : toList child


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------


instance (Eq a) => Eq (MNStack a) where
  Empty        == Empty          =  True
  (Node e _ c) == (Node e' _ c') =  e==e' && c==c'
  _            == _              =  False

--

instance (Show a) => Show (MNStack a) where
  show stk = "MNStack( " ++ intercalate " | " (aux stk) ++ " )"
    where
     aux Empty        =  []
     aux (Node e _ s) =  show e : aux s


---------------------------------------------------------------------------
