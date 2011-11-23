module Subdivision where

import Prelude hiding (null)

import Ants
import Neighbors
import Point
import Path
import Data.Queue

import Data.Vector hiding (fromList, (++), null, map, zip, foldl, any, concat)
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Foldable as F

type Potential = (Division, SmartPoint, Direction, Int)
type SearchGraph = V.Vector [Potential]
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

search :: (SmartPoint -> Bool) -> (SmartPoint -> a -> b) -> Queue (SmartPoint, a) -> Set SmartPoint -> [(SmartPoint, b)]
search fn updateFn queue closed =
  if null queue
  then []
  else let (point, dat) = peek queue
           dat' = updateFn point dat
           queue' = dequeue queue
           (closed', n) = F.foldl (\ acc@(cl, a) x -> if (fn x || S.member x cl) then acc else ((S.insert x cl), (x,dat'):a)) (closed, []) (neighbors point) in
         n ++ (search fn updateFn queue' closed')

subdivide :: GameParams -> World -> DividedWorld
subdivide gp w =
    let q = controlPoints gp w
        fn = (\ x -> isWater.tile $ w ! dumbPoint x) in
       unsafeUpd (V.map (\ x -> (x, WaterDivision)) w) (map (\ (x,i) -> (dumbPoint x, ((w ! dumbPoint x), Division i))) $ search fn seq q S.empty)

makeQueues :: GameParams -> DividedWorld -> [(Division, Potential)]
makeQueues gp dw =
  concat $ map makePotential $ V.foldl (\ acc p -> if hasForeign p then p:acc else acc) [] (smartVector gp)
    where get p = snd $ dw ! dumbPoint p
          hasForeign p = let div = get p in
            div /= WaterDivision && F.any (\ np -> div /= (get np)) (neighbors p)
          makePotential p = let div = get p in
            F.foldl (\ acc np -> 
                      let dir = fst np
                          div' = get $ snd np in
                        if (div == div') || (div' == WaterDivision) then acc else (div, (div', snd np, dir, 1)):acc) [] (withDirections $ neighbors p)


bfsDivision :: GameParams -> DividedWorld -> (Division, Division) -> [(SmartPoint, (Direction, Int))]
bfsDivision gp dw (thisDiv, thatDiv) =
  let vec = smartVector gp
      border = V.filter (\ p -> thisDiv == (get p) && F.any (\ np -> thatDiv == (get np)) (neighbors p)) vec in
   []
  where get p = snd $ dw ! dumbPoint p
