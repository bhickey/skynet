module Subdivision where

import Prelude hiding (null)

import Ants
import GameParams
import Point
import Path
import Data.Queue

import Data.Vector hiding (fromList, (++), null, map, zip)
import qualified Data.Vector as V
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F

controlPoints :: GameParams -> World -> Queue (SmartPoint, Int)
controlPoints gp w =
  let rs = fsqrt $ rows gp
      cs = fsqrt $ cols gp 
      (grid,_) = smartWorld (rows gp, cols gp) in
    fromList $ zip (mapMaybe (f w) [grid r c | r <- [0,rs..rows gp], c <- [0,cs..cols gp]]) [1..]
    where fsqrt = floor.sqrt.fromIntegral
          stopFn w p = not.isWater.tile $ w ! (dumbPoint p)
          f w p = explore hilbert2 (stopFn w) p

search :: (SmartPoint -> Bool) -> Queue (SmartPoint, Int) -> Set SmartPoint -> [(SmartPoint, Int)]
search fn queue closed =
  if null queue
  then []
  else let (sp, divId) = peek queue
           queue' = dequeue queue
           (closed', n) = F.foldl (\ acc@(cl, a) x -> if (fn x || S.member x cl) then acc else ((S.insert x cl), (x,divId):a)) (closed, []) (neighbors sp) in
         n ++ (search fn queue' closed')

subdivide :: GameParams -> World -> DividedWorld
subdivide gp w =
    let q = controlPoints gp w
        fn = (\ x -> isWater.tile $ w ! dumbPoint x) in
      V.fromList $ map (\ (x,i) -> ((w ! dumbPoint x),i)) $ sort $ search fn q S.empty
