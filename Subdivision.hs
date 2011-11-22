module Subdivision where

import Prelude hiding (null)

import Ants
import GameParams
import Point
import Path
import Data.Queue

import Data.Vector hiding (fromList, (++), null, map, zip, foldl)
import qualified Data.Vector as V
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Foldable as F

type DividedWorld = V.Vector (MetaTile, Division)
data Division = Division Int | WaterDivision deriving (Eq, Ord)

controlPoints :: GameParams -> World -> Queue (SmartPoint, Int)
controlPoints gp w =
  let rs = fsqrt $ rows gp
      cs = fsqrt $ cols gp 
      (grid,_) = smartWorld (rows gp, cols gp) in
    fromList $ zip (mapMaybe (f w) [grid r c | r <- [0,rs..rows gp - 1], c <- [0,cs..cols gp - 1]]) [1..]
    where fsqrt = floor.sqrt.fromIntegral
          stopFn w p = not.isWater.tile $ w ! (dumbPoint p)
          f w p = explore hilbert2 (stopFn w) p

allPoints :: GameParams -> [SmartPoint]
allPoints gp = 
  let (grid,_) = smartWorld (rows gp, cols gp) in
    [grid r c | r <- [0..rows gp - 1], c <- [0..cols gp - 1]]

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
       unsafeUpd (V.map (\ x -> (x, WaterDivision)) w) (map (\ (x,i) -> (dumbPoint x, ((w ! dumbPoint x), Division i))) $ search fn q S.empty)

divisionAdjacencies :: DividedWorld -> [SmartPoint] -> Map Division (Set Division)
divisionAdjacencies dw pts = foldl (addDivision dw) M.empty pts
  
addDivision :: DividedWorld -> Map Division (Set Division) -> SmartPoint -> Map Division (Set Division)
addDivision dw acc sp =
  let thisDiv = (snd $ dw ! dumbPoint sp) 
      neighborSet = F.foldl (getForeign dw thisDiv) S.empty (neighbors sp) in
    if S.null neighborSet
    then acc
    else M.alter (\ x -> case x of
                           Nothing -> Just neighborSet
                           Just a -> Just $ S.union a neighborSet) thisDiv acc
  where getForeign w dv acc p = let dv' = (snd $ w ! dumbPoint p) in 
          case (snd $ w ! dumbPoint p) of 
            WaterDivision -> acc
            dv' -> if dv == dv' then acc else S.insert dv' acc


