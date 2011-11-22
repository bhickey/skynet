module Subdivision where

import Prelude hiding (null)

import Ants
import Point
import Path
import Data.Queue

import Data.Vector hiding (fromList, (++), null, map, zip, foldl, any)
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Foldable as F

type SearchGraph = V.Vector [(Division, Direction, Int)]
type DividedWorld = V.Vector (MetaTile, Division)
data Division = Division Int | WaterDivision deriving (Eq, Ord)

controlPoints :: GameParams -> World -> Queue (SmartPoint, Int)
controlPoints gp w =
  let rs = fsqrt $ rows gp
      cs = fsqrt $ cols gp 
      grid = smartGrid gp in
    fromList $ zip (mapMaybe f [grid r c | r <- [0,rs..rows gp - 1], c <- [0,cs..cols gp - 1]]) [1..]
    where fsqrt = floor.sqrt.fromIntegral
          stopFn p = not.isWater.tile $ w ! (dumbPoint p)
          f p = explore hilbert2 stopFn p

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
                        
bfsDivision :: GameParams -> DividedWorld -> (Division, Division) -> [(SmartPoint, (Direction, Int))]
bfsDivision gp dw (thisDiv, thatDiv) =
  let vec = smartVector gp
      border = V.filter (\ p -> thisDiv == (get p) && F.any (\ np -> thatDiv == (get np)) (neighbors p)) vec in
   []
  where get p = snd $ dw ! dumbPoint p
                            
divisionAdjacencies :: DividedWorld -> [SmartPoint] -> Map Division (Set Division)
divisionAdjacencies dw pts = foldl (addDivision dw) M.empty pts
  
addDivision :: DividedWorld -> Map Division (Set Division) -> SmartPoint -> Map Division (Set Division)
addDivision dw acc sp =
  let thisDiv = (snd $ dw ! dumbPoint sp) 
      neighborSet = F.foldl (getForeign thisDiv) S.empty (neighbors sp) in
    if S.null neighborSet
    then acc
    else M.alter (\ x -> case x of
                           Nothing -> Just neighborSet
                           Just a -> Just $ S.union a neighborSet) thisDiv acc
  where getForeign dv acc' p = 
          case (snd $ dw ! dumbPoint p) of 
            WaterDivision -> acc'
            dv' -> if dv == dv' then acc' else S.insert dv' acc'


