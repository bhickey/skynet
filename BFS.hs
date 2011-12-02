module BFS (bfs) where

import Neighbors
import Point

import qualified Data.Foldable as F
import qualified Data.IntSet as I
import qualified Data.Queue as Q
import qualified Data.Vector as V

import System.IO
import System.IO.Unsafe

bfs :: (Show a) => V.Vector a -> (SmartPoint -> Bool) -> (a -> (Direction, SmartPoint) -> a) -> [(SmartPoint, a)] -> V.Vector a
bfs def incl fn pts =
  let delta = bfs' (Q.fromList pts) (I.fromList (map (dumbPoint.fst) pts)) in
    V.unsafeUpd def delta
  where bfs' q closed =
            if Q.null q
            then []
            else let (pt,v) = Q.peek q
                     npts = filter 
                           (\ (_,p) -> (flip I.notMember closed $ dumbPoint p) && (incl p)) 
                           $ F.toList (withDirections $ neighbors pt)
                     dpts = map (dumbPoint.snd) npts
                     vals = map (fn v) npts
                     c' = foldl (flip I.insert) closed dpts
                     q' = Q.enqueueAll (Q.dequeue q) (zip (map snd npts) vals) in
                     seq (unsafePerformIO $ hPutStrLn stderr $ show (zip dpts vals)) $
                   (zip dpts vals) ++ (bfs' q' c')
