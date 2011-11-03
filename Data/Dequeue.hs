{-# LANGUAGE CPP #-}
{- |
Module      :  Data.Dequeue
Description :  A typeclass and an implementation for double-ended queues.
Copyright   :  (c) Henry Bucklow 2009-2010
License     :  BSD3

Maintainer  :  henry@elsie.org.uk
Stability   :  provisional
Portability :  portable

A typeclass for double-ended queues, and an implementation of Banker's
Dequeues, as described in Chris Okasaki's Purely Functional Data Structures.
-}
module Data.Dequeue (
    -- * The 'Dequeue' type class.
    Dequeue(..),
    -- * Support for 'Read' and 'Show' instances
    showDequeue,
    readDequeue,
    -- * QuickCheck properties for 'Dequeue' instances
    prop_pushpop_front,
    prop_pushpop_back,
    prop_push_front,
    prop_push_back,
    prop_takeFront,
    prop_takeBack,
    prop_length_toList,
    prop_fromList_toList,
    -- * Banker's Dequeues
    BankersDequeue,
    -- * QuickCheck properties for 'BankersDequeue'
    prop_pushpop_front_bq,
    prop_pushpop_back_bq,
    prop_push_front_bq,
    prop_push_back_bq,
    prop_takeFront_bq,
    prop_takeBack_bq,
    prop_length_toList_bq,
    prop_fromList_toList_bq,
    prop_push_front_bq_balance,
    prop_push_back_bq_balance,
    prop_pop_front_bq_balance,
    prop_pop_back_bq_balance,
    prop_read_show_bq
) where

import Prelude hiding (foldl, foldr, foldl1, foldr1, length, last)

import Control.Monad
import Data.Foldable
import qualified Data.List as List

import Test.QuickCheck
#if MIN_VERSION_QuickCheck(2,0,0)
#else
   hiding (check)
#endif

import Safe

import qualified Data.Dequeue.Show

-- | A typeclass for double-ended queues.
class Dequeue q where
    -- | Generates an empty queue.
    empty :: q a
    -- | Returns 'True' if this queue is empty.
    null :: q a -> Bool
    -- | Returns the number of elements in this queue.
    length :: q a -> Int
    -- | Returns the item on the front of the queue.
    first :: q a -> Maybe a
    -- | Returns the item on the end of the queue.
    last :: q a -> Maybe a
    -- | Returns the first n items from the front of the queue, in the order
    --   they would be popped.
    takeFront :: Int -> q a -> [a]
    -- | Returns the last n items from the end of the queue, in the order they
    --  would be popped.
    takeBack :: Int -> q a -> [a]
    -- | Pushes an item onto the front of the queue.
    pushFront :: q a -> a -> q a
    -- | Pops an item from the front of the queue.
    popFront :: q a -> (Maybe a, q a)
    -- | Pushes an item onto the back of the queue.
    pushBack :: q a -> a -> q a
    -- | Pops an item from the back of the queue.
    popBack :: q a -> (Maybe a, q a)
    -- | Converts a list into a queue.
    fromList :: [a] -> q a

-- | Support to make generating 'Show' instances for 'Dequeue's easier. Use as
--   follows:
--
--   @
--   instance Show a => Show (MyDequeue a) where
--       show q = showDequeue q
--   @
--
--   The resulting 'Show' instance will be portable between 'Deqeue' instances,
--   and will not expose the details of how your 'Dequeue' instance is
--   constructed.
showDequeue :: (Foldable q, Dequeue q, Show a) => q a -> String
showDequeue q = show $ Data.Dequeue.Show.Dequeue (toList q)

-- | Support to make generating 'Read' instances for 'Dequeue's easier. Use as
--   follows:
--
--   @
--   instance Read a => Read (MyDequeue a) where
--       readsPrec i = readDequeue $ readsPrec i
--   @
--
--   The resulting 'Read' instance will be portable between 'Deqeue' instances,
--   and will not expose the details of how your 'Dequeue' instance is
--   constructed.
readDequeue :: (Dequeue q, Read a) => ReadS (Data.Dequeue.Show.Dequeue a) -> ReadS (q a)
readDequeue readsDefn = \ s -> map convert (readsDefn s)
    where convert (Data.Dequeue.Show.Dequeue values, s) = (fromList values, s)

-- QuickCheck properties for Dequeue instances.

-- | Validates that if you push, then pop, the front of the queue,
--   you get the same queue.
prop_pushpop_front :: (Dequeue q, Eq a, Eq (q a)) => q a -> a -> Bool
prop_pushpop_front q a =
    let (a', q') = popFront (pushFront q a) in
    a' == Just a && q' == q

-- | Validates that if you push, then pop, the back of the queue,
--   you get the same queue.
prop_pushpop_back :: (Dequeue q, Eq a, Eq (q a)) => q a -> a -> Bool
prop_pushpop_back q a =
    let (a', q') = popBack (pushBack q a) in
    a' == Just a && q' == q

-- | Validates that 'first' returns the last 'pushFront''d element.
prop_push_front :: (Dequeue q, Eq a) => q a -> a -> Bool
prop_push_front q a = first (pushFront q a) == Just a

-- | Validates that 'last' returns the last 'pushBack''d element.
prop_push_back :: (Dequeue q, Eq a) => q a -> a -> Bool
prop_push_back q a = last (pushBack q a) == Just a

-- | Validates that the last 'n' pushed elements are returned by takeFront.
prop_takeFront :: (Dequeue q, Eq a) => q a -> [a] -> Bool
prop_takeFront q as =
    takeFront (List.length as) (foldr (flip pushFront) q as) == as

-- | Validates that the last 'n' pushed elements are returned by takeBack.
prop_takeBack :: (Dequeue q, Eq a) => q a -> [a] -> Bool
prop_takeBack q as =
    takeBack (List.length as) (foldr (flip pushBack) q as) == as

-- | Validates that the length of a queue is the same as the length of the
--   list generated from the queue.
prop_length_toList :: (Dequeue q, Foldable q) => q a -> Bool
prop_length_toList q = List.length (toList q) == length q

-- | Validates that fromList . toList is the identity.
prop_fromList_toList :: (Dequeue q, Foldable q, Eq (q a)) => q a -> Bool
prop_fromList_toList q = (fromList . toList) q == q

-- | An implementation of Banker's Dequeues, as described in Chris Okasaki's
--   Purely Functional Data Structures. The functions for the 'Dequeue'
--   instance have the following complexities (where n is the 'length' of the
--   queue):
--
--    * 'length': O(1)
--
--    * 'first': O(1)
--
--    * 'last': O(1)
--
--    * 'takeFront': O(n)
--
--    * 'takeBack': O(n)
--
--    * 'pushFront': O(1) amortised
--
--    * 'popFront': O(1) amortised
--
--    * 'pushBack': O(1) amortised
--
--    * 'popBack': O(1) amortised
--
--    * 'fromList': O(n)
data BankersDequeue a = BankersDequeue Int [a] Int [a]

instance Functor BankersDequeue where
    fmap f (BankersDequeue sizeF front sizeR rear) =
        BankersDequeue sizeF (fmap f front) sizeR (fmap f rear)

instance Foldable BankersDequeue where
    fold (BankersDequeue _ front _ rear) = fold (front ++ reverse rear)
    foldMap f (BankersDequeue _ front _ rear) = foldMap f (front ++ reverse rear)
    foldr f a (BankersDequeue _ front _ rear) = foldr f a (front ++ reverse rear)
    foldl f a (BankersDequeue _ front _ rear) = foldl f a (front ++ reverse rear)
    foldr1 f (BankersDequeue _ front _ rear) = foldr1 f (front ++ reverse rear)
    foldl1 f (BankersDequeue _ front _ rear) = foldl1 f (front ++ reverse rear)

instance Dequeue BankersDequeue where
    empty = BankersDequeue 0 [] 0 []
    null (BankersDequeue 0 [] 0 []) = True
    null _ = False
    length (BankersDequeue sizeF _ sizeR _) = sizeF + sizeR
    first (BankersDequeue _ [] _ [x]) = Just x
    first (BankersDequeue _ front _ _) =  headMay front
    last (BankersDequeue _ [x] _ []) = Just x
    last (BankersDequeue _ _ _ rear) = headMay rear
    takeFront i (BankersDequeue sizeF front _ rear) =
        take i front ++ take (i - sizeF) (reverse rear)
    takeBack i (BankersDequeue _ front sizeR rear) =
        take i rear ++ take (i - sizeR) (reverse front)
    pushFront (BankersDequeue sizeF front sizeR rear) x =
        check $ BankersDequeue (sizeF + 1) (x : front) sizeR rear
    popFront (BankersDequeue _ [] _ []) = (Nothing, empty)
    popFront (BankersDequeue _ [] _ [x]) = (Just x, empty)
    popFront (BankersDequeue _ [] _ _) = error "Queue is too far unbalanced."
    popFront (BankersDequeue sizeF (f : fs) sizeR rear) =
        (Just f, check $ BankersDequeue (sizeF - 1) fs sizeR rear)
    pushBack (BankersDequeue sizeF front sizeR rear) x =
        check $ BankersDequeue sizeF front (sizeR + 1) (x : rear)
    popBack (BankersDequeue _ [] _ []) = (Nothing, empty)
    popBack (BankersDequeue _ [x] _ []) = (Just x, empty)
    popBack (BankersDequeue _ _ _ []) = error "Queue is too far unbalanced."
    popBack (BankersDequeue sizeF front sizeR (r : rs)) =
        (Just r, check $ BankersDequeue sizeF front (sizeR - 1) rs)
    fromList list = check $ BankersDequeue (List.length list) list 0 []

-- | The maximum number of times longer one half of a 'BankersDequeue' is
--   permitted to be relative to the other.
bqBalance :: Int
bqBalance = 4

-- | Checks to see if the queue is too far out of balance. If it is, it
--   rebalances it.
check :: BankersDequeue a -> BankersDequeue a
check q@(BankersDequeue sizeF front sizeR rear)
    | sizeF > c * sizeR + 1 =
        let front' = take size1 front
            rear' = rear ++ reverse (drop size1 front)
        in
        BankersDequeue size1 front' size2 rear'
    | sizeR > c * sizeF + 1 =
        let front' = front ++ reverse (drop size1 rear)
            rear' = take size1 rear
        in
        BankersDequeue size2 front' size1 rear'
    | otherwise = q
    where
        size1 = (sizeF + sizeR) `div` 2
        size2 = (sizeF + sizeR) - size1
        c = bqBalance

instance (Arbitrary a) => Arbitrary (BankersDequeue a) where
    arbitrary = (liftM fromList) arbitrary

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary (BankersDequeue _ front _ rear) =
        variant 0 . coarbitrary front . coarbitrary rear
#endif

instance Eq a => Eq (BankersDequeue a) where
    queue1 == queue2 = toList queue1 == toList queue2

instance Show a => Show (BankersDequeue a) where
    show q = showDequeue q

instance Read a => Read (BankersDequeue a) where
    readsPrec i = readDequeue $ readsPrec i

-- QuickCheck properties for BankersDequeue.

-- | Validates that if you push, then pop, the front of a 'BankersQueue',
--   you get the same queue.
prop_pushpop_front_bq :: BankersDequeue Int -> Int -> Bool
prop_pushpop_front_bq = prop_pushpop_front

-- | Validates that if you push, then pop, the back of a 'BankersDequeue',
--   you get the same queue.
prop_pushpop_back_bq :: BankersDequeue Int -> Int -> Bool
prop_pushpop_back_bq = prop_pushpop_back

-- | Validates that 'first' returns the last 'pushFront''d element.
prop_push_front_bq :: BankersDequeue Int -> Int -> Bool
prop_push_front_bq = prop_push_front

-- | Validates that 'last' returns the last 'pushBack''d element.
prop_push_back_bq :: BankersDequeue Int -> Int -> Bool
prop_push_back_bq = prop_push_back

-- | Validates that the last 'n' pushed elements are returned by takeFront.
prop_takeFront_bq :: BankersDequeue Int -> [Int] -> Bool
prop_takeFront_bq = prop_takeFront

-- | Validates that the last 'n' pushed elements are returned by takeBack.
prop_takeBack_bq :: BankersDequeue Int -> [Int] -> Bool
prop_takeBack_bq = prop_takeBack

-- | Validates that the length of a 'BankersDequeue' is the same as the length
--   of the list generated from the queue.
prop_length_toList_bq :: BankersDequeue Int -> Bool
prop_length_toList_bq = prop_length_toList

-- | Validates that fromList . toList is the identity for a 'BankersDequeue'.
prop_fromList_toList_bq :: BankersDequeue Int -> Bool
prop_fromList_toList_bq = prop_fromList_toList

balanced :: BankersDequeue a -> Bool
balanced (BankersDequeue 0 _ 0 _) = True
balanced (BankersDequeue 1 _ 0 _) = True
balanced (BankersDequeue 0 _ 1 _) = True
balanced (BankersDequeue sizeF _ sizeR _) =
    sizeF <= bqBalance * sizeR + 1 && sizeR <= bqBalance * sizeF + 1

-- | Validates that a 'BankersDequeue' remains balanced despite repeated
--   pushes to the front.
prop_push_front_bq_balance :: BankersDequeue Int -> Int -> Bool
prop_push_front_bq_balance q count =
    let push _ = (flip pushFront) 0
        q' = foldr push q [0 .. count] in
    balanced q'

-- | Validates that a 'BankersDequeue' remains balanced despite repeated
--   pushes to the back.
prop_push_back_bq_balance :: BankersDequeue Int -> Int -> Bool
prop_push_back_bq_balance q count =
    let push _ = (flip pushBack) 0
        q' = foldr push q [0 .. count] in
    balanced q'

-- | Validates that a 'BankersDequeue' remains balanced despite repeated
--   pops from the front.
prop_pop_front_bq_balance :: BankersDequeue Int -> Int -> Bool
prop_pop_front_bq_balance q count =
    let pop _ queue = let (_, queue') = popFront queue in queue'
        q' = foldr pop q [0 .. count] in
    balanced q'

-- | Validates that a 'BankersDequeue' remains balanced despite repeated
--   pops from the back.
prop_pop_back_bq_balance :: BankersDequeue Int -> Int -> Bool
prop_pop_back_bq_balance q count =
    let pop _ queue = let (_, queue') = popBack queue in queue'
        q' = foldr pop q [0 .. count] in
    balanced q'

-- | Validates that a 'BankersDequeue' has read and show instances that are
--   the inverse of each other.
prop_read_show_bq :: BankersDequeue Int -> Bool
prop_read_show_bq q = (read . show) q == q
