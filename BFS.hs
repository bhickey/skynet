module BFS (bfs) where

import Neighbors
import Point

import qualified Data.Foldable as F

import qualified Data.IntSet as I
import qualified Data.Queue as Q
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)

bfs :: (a -> (Direction, SmartPoint) -> a) -> [(SmartPoint, a)] -> Vector a
bfs fn pts =
  V.fromList $ (map snd) $ (sortBy comparator) 
  $ bfs' (Q.fromList pts) (I.fromList (map (dumbPoint.fst) pts))
  where comparator x y = compare (fst x) (fst y)
        bfs' q closed =
            if Q.null q
            then []
            else let (pt,v) = Q.peek q
                     npts = filter 
                           (\ (_,p) -> flip I.notMember closed $ dumbPoint p) 
                           $ F.toList (withDirections $ neighbors pt)
                     dpts = map (dumbPoint.snd) npts
                     vals = map (fn v) npts
                     c' = foldl (flip I.insert) closed dpts
                     q' = Q.enqueueAll (Q.dequeue q) (zip (map snd npts) vals) in
                   (zip dpts vals) ++ (bfs' q' c')
