module BFS (bfs) where

import Neighbors
import Point

import Data.List (permutations)

import qualified Data.Foldable as F

import qualified Data.IntSet as I
import qualified Data.Queue as Q
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)

bfs :: (a -> (Direction, SmartPoint) -> a) -> [(SmartPoint, a)] -> Vector a
bfs fn pts =
  V.fromList $ (map snd) $ (sortBy comparator) $ bfs' (Q.fromList (zipWith tuple2 winds pts)) I.empty
  where comparator x y = compare (fst x) (fst y)
        winds = cycle $ concat $ permutations [North,South,East,West]
        tuple1 (a,b) z = (a,b,z)
        tuple2 a (x,y) = (a,x,y)
        bfs' q closed =
            if Q.null q
            then []
            else let (dir,pt,v) = Q.peek q
                     pts = filter 
                           (\ (_,p) -> flip I.notMember closed $ dumbPoint p) 
                           $ F.toList (withDirections $ neighbors pt)
                     dpts = map (dumbPoint.snd) pts
                     vals = map (fn v) pts
                     c' = foldl (flip I.insert) closed dpts
                     q' = Q.enqueueAll q (zipWith tuple1 pts vals) in
                   (zip dpts vals) ++ (bfs' q' c')
