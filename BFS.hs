module BFS (bfs) where

import Neighbors
import Point

import qualified Data.Foldable as F

import qualified Data.IntSet as I
import qualified Data.Queue as Q
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)

bfs :: ((SmartPoint, a) -> SmartPoint -> a) -> [(SmartPoint, a)] -> Vector a
bfs fn pts =
  V.fromList $ (map snd) $ (sortBy comparator) $ bfs' (Q.fromList pts) I.empty
  where comparator x y = compare (fst x) (fst y)
        bfs' q closed =
            if Q.null q
            then []
            else let top = Q.peek q
                     pts = filter 
                           (\ p -> flip I.notMember closed $ dumbPoint p) 
                           $ F.toList (neighbors (fst top))
                     dpts = map dumbPoint pts
                     vals = map (fn top) pts
                     c' = foldl (flip I.insert) closed dpts
                     q' = Q.enqueueAll q (zip pts vals) in
                     (zip dpts vals) ++ (bfs' q' c')
