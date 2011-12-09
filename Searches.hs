module Searches where

import Ants
import BFS
import Point
import Neighbors

import System.Random.Shuffle
import Data.Vector ((!), Vector)
import qualified Data.Vector as V

skipWater :: GameState -> SmartPoint -> Bool
skipWater gs p = (not.isWater.tile) ((world gs) ! (dumbPoint p))

skipAnt :: GameState -> SmartPoint -> Bool
skipAnt gs p = (not.isLiveAnt.tile) ((world gs) ! (dumbPoint p))

skipFriendly :: GameState -> SmartPoint -> Bool
skipFriendly gs p = (not.isLiveFriendlyAnt.tile) ((world gs) ! (dumbPoint p))

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

nearestUnseen :: GameParams -> GameState -> V.Vector (Maybe (Int, Direction))
nearestUnseen gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (vectorOf gs Nothing)
    (skipWater gs)
    neverSkip
    searchFn
    (unsafeShuffle
      (zip (V.toList $ V.filter (\ v -> isUnobserved $ w ! (dumbPoint v)) sv) (cycle [Just (0, North)])))
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
  
