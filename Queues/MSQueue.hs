------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23s23                                            --
--   Copyright 2024 Serafín (@sfmolina)                         --
------------------------------------------------------------------
--[ Implementing an improved basic linear queue with two stacks


module SFDataStructures.Queues.MSQueue (
    MQueue,
    empty,
    enqueue,
    dequeue,
    isEmpty,
    first,
    size,
    fromList,
    toList
) where

import qualified SFDataStructures.Stacks.MNStack as Stk
import Data.List ( intercalate )


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DATA DEFINITION --------------------------------------------------------


data MQueue a = Q (Stk.MStack a) (Stk.MStack a)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MQueue a
empty = Q Stk.empty Stk.empty

--

mkValid :: Stk.MStack a -> Stk.MStack a -> MQueue a
mkValid s1 s2
    | Stk.isEmpty s1 = Q s1' s2'
    | otherwise      = Q s1  s2
    where
        (s1', s2') = stkReverse s1 s2
        stkReverse a b
            | Stk.isEmpty b = (a, b)
            | otherwise     = stkReverse a' b'
            where
                a' = Stk.push (Stk.top b) a
                b' = Stk.pop b

--

enqueue :: a -> MQueue a -> MQueue a
enqueue x (Q s1 s2) = mkValid s1 (Stk.push x s2)

--

fromList :: [a] -> MQueue a
fromList = foldl func empty
    where
        func acc x = enqueue x acc 


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


dequeue :: MQueue a -> MQueue a
dequeue q@(Q s1 s2)
    | isEmpty q      = error "ERROR: dequeue on empty queue"
    | otherwise      = mkValid (Stk.pop s1) s2


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


isEmpty :: MQueue a -> Bool
isEmpty (Q s1 s2)
    | Stk.isEmpty s1 = True
    | otherwise      = False

--

first :: MQueue a -> a
first q@(Q s1 s2)
    | isEmpty q      = error "ERROR: first on empty queue"
    | otherwise = Stk.top s1

--

size :: MQueue a -> Int
size (Q s1 s2) = Stk.size s1 + Stk.size s2

--

toList :: MQueue a -> [a]
toList queue
    | isEmpty queue = []
    | otherwise     = first queue : toList (dequeue queue)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------


instance (Eq a) => Eq (MQueue a) where
  (==) :: Eq a => MQueue a -> MQueue a -> Bool
  q == q' = (toList q) == (toList q')

--

instance (Show a) => Show (MQueue a) where
  show :: Show a => MQueue a -> String
  show queue = "MQueue( " ++ intercalate " | " (aux queue) ++ " )"
    where
     aux q
        | isEmpty q = []
        | otherwise = show (first q) : aux (dequeue q)


---------------------------------------------------------------------------
