module BFS (bfs) where

import Neighbors
import Point

import qualified Data.Foldable as F
import qualified Data.IntSet as I
import qualified Data.Queue as Q
import Data.Vector ((//))
import qualified Data.Vector as V

bfs :: V.Vector a 
   -> (SmartPoint -> SmartPoint -> Bool)
   -> (SmartPoint -> Bool)
   -> (a -> (Direction, SmartPoint) -> a)
   -> [(SmartPoint, a)]
   -> V.Vector a
bfs def incl use fn pts =
  let delta = bfs' (Q.fromList pts) (I.fromList (map (dumbPoint.fst) pts)) in
    def // delta
  where bfs' q c =
          if Q.null q
          then []
          else let (pt, v) = Q.peek q
                   npts = filter (\ (_, p) -> I.notMember (dumbPoint p) c && incl p pt) $
                     F.toList $ (withDirections.neighbors) pt
                   spts = map snd npts
                   dpts = map dumbPoint spts
                   vs = map (fn v) npts
                   c' = foldl (flip I.insert) c dpts
                   q' = Q.enqueueAll (Q.dequeue q) (filter (use.fst) (zip spts vs)) in
            (zip dpts vs) ++ (bfs' q' c')
