--
-- Copyright (c) 2009 - 2010 Brendan Hickey - http://bhickey.net
-- New BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Heap.Skew 
(Heap, head, tail, merge, singleton, empty, null, fromList, toList, insert) 
where

import Prelude hiding (head, tail, null)

data (Ord a) => Heap a =
    Leaf
  | Heap a (Heap a) (Heap a) deriving (Eq, Ord)

empty :: (Ord a) => Heap a
empty = Leaf

null :: (Ord a) => Heap a -> Bool
null Leaf = True
null _ = False

singleton :: (Ord a) => a -> Heap a
singleton n = Heap n Leaf Leaf

insert :: (Ord a) => a -> Heap a -> Heap a
insert a h = merge h (singleton a)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Leaf n = n
merge n Leaf = n
merge h1 h2 = foldl1 assemble $ reverse $ listMerge head (cutRight h1) (cutRight h2)

listMerge :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
listMerge _ [] s = s
listMerge _ f [] = f
listMerge c f@(h1:t1) s@(h2:t2) =
  if c h1  <= c h2
  then h1 : listMerge c t1 s
  else h2 : listMerge c f t2

cutRight :: (Ord a) => Heap a -> [Heap a]
cutRight Leaf = []
cutRight (Heap a l r) =  Heap a l Leaf : cutRight r

-- assumes h1 >= h2, merge relies on this
assemble :: (Ord a) => Heap a -> Heap a -> Heap a
assemble h1 (Heap a l Leaf) = Heap a h1 l
assemble _ _ = error "invalid heap assembly"

head :: (Ord a) => Heap a -> a
head Leaf = error "head of empty heap"
head (Heap a _ _) = a

tail :: (Ord a) => Heap a -> Heap a
tail Leaf = error "tail of empty heap"
tail (Heap _ l r) = merge l r

toList :: (Ord a) => Heap a -> [a]
toList Leaf = []
toList (Heap n l r) = n : toList (merge l r)

fromList :: (Ord a) => [a] -> Heap a
fromList [] = Leaf
fromList l = mergeList (map singleton l)
              where mergeList [a] = a
                    mergeList x = mergeList (mergePairs x)
                    mergePairs (a:b:c) =  merge a b : mergePairs c
                    mergePairs x = x
