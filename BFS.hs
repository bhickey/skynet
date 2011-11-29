module BFS where

import Neighbors
import Point

import qualified Data.Foldable as F

import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.Queue (Queue)
import qualified Data.Queue as Q
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)

bfs :: ((Direction, SmartPoint) -> (Int, b)) -> [SmartPoint] -> Vector b
bfs fn pts =
  V.fromList $ (map snd) $ (sortBy comparator) $ bfs' (Q.fromList pts) I.empty
  where comparator x y = compare (fst x) (fst y)
        bfs' q closed =
            if Q.null q
            then []
            else let top = Q.peek q
                     n = filter 
                         (\ (_,p) -> flip I.notMember closed $ dumbPoint p) 
                         $ F.toList (withDirections $ neighbors top)
                     c' = foldl (\ s (_,p) -> I.insert (dumbPoint p) s) closed n
                     q' = Q.enqueueAll q (map snd n) in
                   (map fn n) ++ (bfs' q' c')
