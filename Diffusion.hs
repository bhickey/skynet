module Diffusion where
                  
import Data.Array.Diff
import Data.Ix
import Data.Maybe
import Data.List as L

import Point
import Ants

aimap :: (Ix i) => (Array i e) -> (i -> e -> c) -> (Array i c)
aimap a f = listArray (bounds a) $ fmap (uncurry f) (assocs a)

data Automata = Automata
  { friendlyAnt :: Int
  , enemyAnt :: Int
  , foodProb :: Float
  , friendlyHill :: Float
  , enemyHill :: Float
  , water :: Bool
  }

emptyAutomata = Automata 0 0 0.0 0.0 0.0 False
waterAutomata = Automata 0 0 0.0 0.0 0.0 True

tileToEnum :: Tile -> Automata
tileToEnum (AntTile Me)  = Automata 1 0 0.0 0.0 0.0 False 
tileToEnum (AntTile _)   = Automata 0 1 0.0 0.0 0.0 False
tileToEnum (HillTile Me) = Automata 0 0 0.0 1.0 0.0 False
tileToEnum (HillTile _)  = Automata 0 0 0.0 0.0 1.0 False
tileToEnum FoodTile      = Automata 0 0 1.0 0.0 0.0 False
tileToEnum Water         = waterAutomata
tileToEnum _             = emptyAutomata

type DiffusionGrid = Array Point Automata
type Rule = (Automata -> [Automata] -> Automata)

rule (Automata _ _ _  _ _ True) _ = waterAutomata
rule (Automata a _ fD _ _ False) tiles = 
  let denom = if a == 1 then 4.0 else 2.0
      fD' = max fD ((foldl1 max $ map foodProb tiles) / denom) in
    Automata 0 0 fD' 0.0 0.0 False

diffusionGrid :: ImputedWorld -> DiffusionGrid
diffusionGrid w = amap tileToEnum w

applyRules :: DiffusionGrid -> Rule -> Point -> Automata -> Automata
applyRules grid rule i e =
  let n = (map (grid !) (neighbors i)) in
   rule e n

bestScore :: DiffusionGrid -> Point -> [Direction]
bestScore dg p =
    let points = zip (map foodProb (map (dg !) (map (neighbor p) directions))) directions in
      map snd $ L.sortBy (\ (a,_) (b,_) -> compare b a) points
   
diffuse :: Rule -> DiffusionGrid -> DiffusionGrid
diffuse r dg = aimap dg (applyRules dg r)
