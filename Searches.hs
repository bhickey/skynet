module Searches where

import Ants
import BFS
import Point

import qualified Data.Vector as V

ownership :: GameState -> V.Vector Owner
ownership gs =
  let w = world gs in bfs 
    (\ (_,a) _ -> a) 
    (map (\ a -> (pointAnt a, ownerAnt a)) $ ants gs)    

nearestFood :: GameState -> V.Vector Direction
nearestFood gs =
  let w = world gs in
    bfs 
    (\ (p0,_) p1 -> a) 
    (map (\ a -> (pointAnt a, ownerAnt a)) $ ants gs)    
