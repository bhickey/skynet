module Searches where

import Ants
import BFS
import Point
import Neighbors

import Data.Vector ((!))
import qualified Data.Vector as V

ownership :: GameState -> V.Vector Ant
ownership gs = bfs (\ a _ -> a) 
    (map (\ a -> (pointAnt a, a)) (ants gs))

nearestFood :: GameState -> V.Vector Ant -> V.Vector (Food, Ant, Direction)
nearestFood gs av = bfs
  (\ (f, a, _) (d, _) -> (f, a, fromDirection d))
  (map (\ f -> (f, (f, av ! (dumbPoint f), North))) (food gs))

nearestEnemy :: GameState -> V.Vector (Ant, Direction)
nearestEnemy gs = bfs
  (\ (a, _) (d, _) -> (a, fromDirection d))
  (map (\ a -> (pointAnt a, (a, North))) (enemyAnts $ ants gs))

nearestUnseen :: GameParams -> GameState -> V.Vector Direction
nearestUnseen gp gs = let
  sv = smartVector gp
  w = world gs in
    bfs
    (\ _ (d, _) -> d)
    (zip (V.toList $ V.filter (\ v -> isUnobserved $ w ! (dumbPoint v)) sv) (cycle [North, South, East, West]))
