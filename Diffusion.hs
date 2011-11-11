module Diffusion where
          
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST

import Data.List as L

import Point
import Ants

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

itemToEnum :: Item -> Automata
itemToEnum (LiveAntItem Me)  = Automata 1 0 0 0.0 0.0
itemToEnum (LiveAntItem _)   = Automata 0 1 0 0.0 0.0
itemToEnum (DeadAntItem _)   = emptyAutomata
itemToEnum (HillItem Me)     = Automata 0 0 0 1.0 0.0
itemToEnum (HillItem _)      = Automata 0 0 0 0.0 1.0
itemToEnum FoodItem          = Automata 0 0 100 0.0 0.0
itemToEnum BlankItem         = emptyAutomata

tileToEnum :: Tile -> Automata
tileToEnum WaterTile       = waterAutomata
tileToEnum (LandTile item) = itemToEnum item
tileToEnum UnknownTile     = emptyAutomata

toAutomataPair :: Tile -> AutomataPair
toAutomataPair t = let t' = tileToEnum t in (t',t')

-- a pair of automata, the first is fresh, the second stale
type AutomataPair = (Automata, Automata)
type Rule = AutomataPair -> [AutomataPair] -> AutomataPair

rule :: Rule
rule w@(_,WaterAutomata) _ = w
rule (x, Automata a _ fD _ _) tiles = 
  let penalty = if a == 0 then 1 else 5
      fD' = foldl (\ f n -> max f ((.) foodProb snd n - penalty)) fD tiles in
    (Automata 0 0 fD' 0.0 0.0, x)

applyRules g = mapM (applyRule g rule) (indices g)

applyRule grid r p = do
    t <- readArray grid p
    n <- mapM (readArray grid) (neighbors p)
    writeArray grid p (r t n)
    return grid

bestScore g p = sort $ zipWith (\ a d -> ((.) foodProb (g !) a, d)) (neighbors p) directions

{-
diffuse iw steps = runSTArray $ do
    dg <- newListArray (bounds iw) (map toAutomataPair (elems iw))
    (replicateM_ steps (applyRules dg)) >> return dg -}
