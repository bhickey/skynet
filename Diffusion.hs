module Diffusion where
                  
import Data.Array.Diff
import Data.List as L

import Point
import Ants

aimap :: (Ix i) => (Array i e) -> (i -> e -> c) -> (Array i c)
aimap a f = listArray (bounds a) $ fmap (uncurry f) (assocs a)

data Automata = Automata
  { friendlyAnt :: Int
  , enemyAnt :: Int
  , foodProb :: Int
  , friendlyHill :: Float
  , enemyHill :: Float
  , water :: Bool
  } deriving (Show)

emptyAutomata :: Automata
emptyAutomata = Automata 0 0 0 0.0 0.0 False

waterAutomata :: Automata
waterAutomata = Automata 0 0 0 0.0 0.0 True

tileToEnum :: Tile -> Automata
tileToEnum (AntTile Me)  = Automata 1 0 0 0.0 0.0 False 
tileToEnum (AntTile _)   = Automata 0 1 0 0.0 0.0 False
tileToEnum (HillTile Me) = Automata 0 0 0 1.0 0.0 False
tileToEnum (HillTile _)  = Automata 0 0 0 0.0 1.0 False
tileToEnum FoodTile      = Automata 0 0 100 0.0 0.0 False
tileToEnum Water         = waterAutomata
tileToEnum _             = emptyAutomata

type DiffusionGrid = Array Point Automata
type Rule = (Automata -> [Automata] -> Automata)

rule :: Rule
rule (Automata _ _ _  _ _ True) _ = waterAutomata
rule (Automata a _ fD _ _ False) tiles = 
  let penalty = if a > 0 then 4 else 1
      fD' = max fD (max 0 ((foldl1 max $ map foodProb tiles) - penalty)) in
    Automata 0 0 fD' 0.0 0.0 False

diffusionGrid :: ImputedWorld -> DiffusionGrid
diffusionGrid w = amap tileToEnum w

applyRules :: DiffusionGrid -> Rule -> Point -> Automata -> Automata
applyRules grid r i e =
  let n = (map (grid !) (neighbors i)) in
   (r e n)

bestScore :: DiffusionGrid -> Point -> [Direction]
bestScore dg p =
    let points = zip (map foodProb (map (dg !) (map (neighbor p) directions))) directions in
      (map snd $ L.sortBy (\ (a,_) (b,_) -> compare a b) points)
   
diffuse :: Rule -> DiffusionGrid -> DiffusionGrid
diffuse r dg = aimap dg (applyRules dg r)
