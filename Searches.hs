module Searches where

import Ants
import BFS
import Point
import Neighbors

import Data.Vector ((!), Vector)
import qualified Data.Vector as V

skipWater :: GameState -> SmartPoint -> SmartPoint -> Bool
skipWater gs p _ = (not.isWater.tile) ((world gs) ! (dumbPoint p))

skipAnt :: GameState -> SmartPoint -> Bool
skipAnt gs p = (not.isLiveAnt.tile) ((world gs) ! (dumbPoint p))

skipFriendly :: GameState -> SmartPoint -> Bool
skipFriendly gs p = (not.isLiveFriendlyAnt.tile) ((world gs) ! (dumbPoint p))

skipToUnseen :: GameState -> SmartPoint -> SmartPoint -> Bool
skipToUnseen gs dest orig =
  ((not.isUnobserved) ((world gs) ! (dumbPoint orig))) && (isUnobserved ((world gs) ! (dumbPoint dest))) 
  && (skipWater gs dest orig)

neverSkip :: SmartPoint -> Bool
neverSkip _ = True

vectorOf :: GameState -> a -> Vector a
vectorOf gs a = V.map (\ _ -> a) (world gs) 

ownership :: GameState -> V.Vector Ant
ownership gs = bfs 
    (vectorOf gs NullAnt)
    (skipWater gs)
    (\ _ -> True)
    (\ a _ -> a)
    (map (\ a -> (pointAnt a, a)) (ants gs))

nearestFood :: GameState -> V.Vector Ant -> V.Vector (Maybe (Food, Ant, Int, Direction))
nearestFood gs av = bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  (skipAnt gs)
  searchFn
  (map (\ f -> (f, Just (f, av ! (dumbPoint f), 0, North))) (food gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (f, a, dist, _)) (d, _) = Just (f, a, dist + 1, fromDirection d)

nearestEnemy :: GameState -> V.Vector (Maybe (Ant, Int, Direction))
nearestEnemy gs = bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  (skipFriendly gs)
  searchFn
  (map (\ a -> (pointAnt a, Just (a, 0, North))) (enemyAnts $ ants gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (a, dist, _)) (d, _) = Just (a, dist + 1, fromDirection d)

nearestUnseen :: GameParams -> GameState -> V.Vector (Maybe (SmartPoint, Int, Direction))
nearestUnseen gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (vectorOf gs Nothing)
    (skipWater gs)
    neverSkip
    searchFn
    (zipWith 
      (\ a (b,c) -> (a, Just (a, b, c))) 
      (V.toList $ V.filter (\ v -> isUnobserved $ w ! (dumbPoint v)) sv)
      (cycle [(0, North)]))
    where searchFn Nothing _ = Nothing
          searchFn (Just (pt, dist, _)) (d, _) = Just (pt, dist + 1, fromDirection d)

toPoints:: GameState -> [(SmartPoint, Ant)] -> V.Vector (Maybe (Ant, Int, Direction))
toPoints gs pts =
  bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  neverSkip
  searchFn
  (map (\ (p,a) -> (p, Just (a, 0, North))) pts)
  where searchFn Nothing _ = Nothing
        searchFn (Just (a, dist, _)) (d, _) = Just (a, dist + 1, fromDirection d)

nearestUnknown :: GameParams -> GameState -> V.Vector (Maybe (Int, Direction))
nearestUnknown gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (vectorOf gs Nothing)
    (skipWater gs)
    neverSkip
    searchFn
    (zip (V.toList $ V.filter (\ v -> (isUnknown $ w ! (dumbPoint v))) sv) (cycle [Just (0, North)]))
    where searchFn Nothing _ = Nothing
          searchFn (Just (dist, _)) (d, _) = Just (dist + 1, fromDirection d)
 
nearestPerimeter :: GameParams -> GameState -> V.Vector (Maybe (Int, Direction))
nearestPerimeter gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (vectorOf gs Nothing)
    (skipToUnseen gs)
    (skipAnt gs)
    searchFn
    ((map (\ h -> (pointHill h, Just (0, North))) (enemyHills $ hills gs)) ++
     (zip (V.toList $ V.filter (\ v -> (isUnknown $ w ! (dumbPoint v))) sv) (cycle [Just (0, North)])))
    where searchFn Nothing _ = Nothing
          searchFn (Just (dist, _)) (d, _) = Just (dist + 1, fromDirection d)
 
nearestHill :: GameState -> Vector (Maybe (Hill, Int, Direction))
nearestHill gs =
  bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  (skipFriendly gs)
  searchFn
  (map (\ h -> (pointHill h, Just (h, 0, North))) (enemyHills $ hills gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (a, dist, _)) (d, _) = Just (a, dist + 1, fromDirection d)
