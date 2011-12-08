module Searches where

import Ants
import BFS
import Point
import Neighbors

import Data.Vector ((!), Vector)
import qualified Data.Vector as V

skipWater :: GameState -> SmartPoint -> Bool
skipWater gs p = (not.isWater.tile) ((world gs) ! (dumbPoint p))

vectorOf :: GameState -> a -> Vector a
vectorOf gs a = V.map (\ _ -> a) (world gs) 

ownership :: GameState -> V.Vector Ant
ownership gs = bfs 
    (vectorOf gs NullAnt)
    (skipWater gs)
    (\ a _ -> a)
    (map (\ a -> (pointAnt a, a)) (ants gs))

nearestFood :: GameState -> V.Vector Ant -> V.Vector (Maybe (Food, Ant, Int, Direction))
nearestFood gs av = bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  searchFn
  (map (\ f -> (f, Just (f, av ! (dumbPoint f), 0, North))) (food gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (f, a, dist, _)) (d, _) = Just (f, a, dist + 1, fromDirection d)

nearestEnemy :: GameState -> V.Vector (Maybe (Ant, Int, Direction))
nearestEnemy gs = bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  searchFn
  (map (\ a -> (pointAnt a, Just (a, 0, North))) (enemyAnts $ ants gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (a, dist, _)) (d, _) = Just (a, dist + 1, fromDirection d)

nearestUnseen :: GameParams -> GameState -> V.Vector (Maybe Direction)
nearestUnseen gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (vectorOf gs Nothing)
    (skipWater gs)
    (\ _ (d, _) -> Just (fromDirection d))
    (zip (V.toList $ V.filter (\ v -> isUnobserved $ w ! (dumbPoint v)) sv) (cycle [Just North]))

nearestHill :: GameState -> Vector (Maybe (Hill, Int, Direction))
nearestHill gs =
  bfs
  (vectorOf gs Nothing)
  (skipWater gs)
  searchFn
  (map (\ h -> (pointHill h, Just (h, 0, North))) (enemyHills $ hills gs))
  where searchFn Nothing _ = Nothing
        searchFn (Just (a, dist, _)) (d, _) = Just (a, dist + 1, fromDirection d)
  
