------------------------------------------------------------------
--  AUTHOR:    @sfmolina                                        --
--  Version:   v2                                               --
--  Modified:  23f24                                            --
--  Copyright 2024 Serafín (@sfmolina)                          --
------------------------------------------------------------------
--[ Implementing an improved basic linear queue with two stacks


module SFDataStructures.Queues.MSQueue (
    MSQueue,
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


data MSQueue a = Q (Stk.MNStack a) (Stk.MNStack a)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- CONSTRUCTOR ------------------------------------------------------------


empty :: MSQueue a
empty = Q Stk.empty Stk.empty

--

mkValid :: Stk.MNStack a -> Stk.MNStack a -> MSQueue a
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

enqueue :: a -> MSQueue a -> MSQueue a
enqueue x (Q s1 s2) = mkValid s1 (Stk.push x s2)

--

fromList :: [a] -> MSQueue a
fromList = foldl func empty
    where
        func acc x = enqueue x acc 


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- TRANSFORMER ------------------------------------------------------------


dequeue :: MSQueue a -> MSQueue a
dequeue q@(Q s1 s2)
    | isEmpty q      = error "ERROR: dequeue on empty queue"
    | otherwise      = mkValid (Stk.pop s1) s2


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- SELECTOR ---------------------------------------------------------------


isEmpty :: MSQueue a -> Bool
isEmpty (Q s1 _)
    | Stk.isEmpty s1 = True
    | otherwise      = False

--

first :: MSQueue a -> a
first q@(Q s1 _)
    | isEmpty q      = error "ERROR: first on empty queue"
    | otherwise = Stk.top s1

--

size :: MSQueue a -> Int
size (Q s1 s2) = Stk.size s1 + Stk.size s2

--

toList :: MSQueue a -> [a]
toList queue
    | isEmpty queue = []
    | otherwise     = first queue : toList (dequeue queue)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- INSTANCES --------------------------------------------------------------


instance (Eq a) => Eq (MSQueue a) where
  q == q' = toList q == toList q'

--

instance (Show a) => Show (MSQueue a) where
  show queue = "MSQueue( " ++ intercalate " | " (aux queue) ++ " )"
    where
        aux :: (Show a) => MSQueue a -> [String]
        aux q
            | isEmpty q = []
            | otherwise = show (first q) : aux (dequeue q)


---------------------------------------------------------------------------
