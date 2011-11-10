module Diffusion where
                  
import Data.Array.Diff
import Data.List as L

import Point
import Ants

aimap :: (Ix i) => (Array i e) -> (i -> e -> c) -> (Array i c)
aimap a f = listArray (bounds a) $ fmap (uncurry f) (assocs a)

data Automata = WaterAutomata |
                Automata Int Int Int Float Float deriving (Show)

friendlyAnt :: Automata -> Int
friendlyAnt WaterAutomata = 0
friendlyAnt (Automata f _ _ _ _) = f

enemyAnt :: Automata -> Int
enemyAnt WaterAutomata = 0
enemyAnt (Automata _ e _ _ _) = e

foodProb :: Automata -> Int
foodProb WaterAutomata = 0
foodProb (Automata _ _ fp _ _) = fp

friendlyHill :: Automata -> Float
friendlyHill WaterAutomata = 0
friendlyHill (Automata _ _ _ fh _) = fh

enemyHill :: Automata -> Float
enemyHill WaterAutomata = 0
enemyHill (Automata _ _ _ _ eh) = eh


emptyAutomata :: Automata
emptyAutomata = Automata 0 0 0 0.0 0.0

waterAutomata :: Automata
waterAutomata = WaterAutomata

tileToEnum :: Tile -> Automata
tileToEnum (AntTile Me)  = Automata 1 0 0 0.0 0.0
tileToEnum (AntTile _)   = Automata 0 1 0 0.0 0.0
tileToEnum (HillTile Me) = Automata 0 0 0 1.0 0.0
tileToEnum (HillTile _)  = Automata 0 0 0 0.0 1.0
tileToEnum FoodTile      = Automata 0 0 100 0.0 0.0
tileToEnum Water         = waterAutomata
tileToEnum _             = emptyAutomata

type DiffusionGrid = Array Point Automata
type Rule = (Automata -> [Automata] -> Automata)

rule :: Rule
rule WaterAutomata _ = waterAutomata
rule (Automata a _ fD _ _) tiles =
  let penalty = if a > 0 then 4 else 1
      fD' = max fD (max 0 ((foldl1 max $ map foodProb tiles) - penalty)) in
    Automata 0 0 fD' 0.0 0.0

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
