module Diffusion where
                  
import Data.Array.Diff
import Data.Ix
import Point
import Data.Maybe
import Ants

aimap :: (Ix i) => (Array i e) -> (i -> e -> c) -> (Array i c)
aimap a f = listArray (bounds a) $ fmap (uncurry f) (assocs a)

data Automata = FriendlyAnt | EnemyAnt | Food | MyHill | EnemyHill deriving (Bounded, Eq, Enum, Ix, Ord)

emptyAutomata = array (FriendlyAnt, EnemyHill) $ zip [FriendlyAnt .. EnemyHill] (repeat 0.0)

tileToEnum :: Tile -> AutomataArray
tileToEnum (AntTile Me)  = emptyAutomata // [(FriendlyAnt, 1.0)]
tileToEnum (AntTile _)   = emptyAutomata // [(EnemyAnt, 1.0)]
tileToEnum (HillTile Me) = emptyAutomata // [(MyHill, 1.0)]
tileToEnum (HillTile _)  = emptyAutomata // [(EnemyAnt, 1.0)]
tileToEnum FoodTile  = emptyAutomata // [(Food, 1.0)]
tileToEnum _ = emptyAutomata

type AutomataArray = Array Automata Float
type DiffusionGrid = Array Point AutomataArray
type Rule = (Int, (Float -> Float -> Float))
type DiffusionRules = Array Automata (Maybe Rule)

updateRule :: Maybe Rule -> Maybe Rule
updateRule (Just (1, _)) = Nothing
updateRule (Just (x, f)) = Just (x - 1, f)
updateRule Nothing = Nothing

updateRules :: DiffusionRules -> DiffusionRules
updateRules = amap updateRule

diffusionGrid :: ImputedWorld -> DiffusionGrid
diffusionGrid w = amap tileToEnum w

getRule :: DiffusionRules -> Automata -> (Float -> Float -> Float)
getRule dr auto = 
  case dr ! auto of
    Nothing -> (\ _ _ -> 0.0)
    Just (_, f) -> f

mapRules :: DiffusionRules -> AutomataArray -> AutomataArray -> AutomataArray
mapRules dr a b = aimap a (\ i e -> (getRule dr i) e (b ! i))

applyRules :: DiffusionGrid -> DiffusionRules -> Point -> AutomataArray -> AutomataArray
applyRules grid rules i e =
  let n = (grid ! i):(map (grid !) (neighbors grid i)) in
   foldl1 (mapRules rules) n
    
diffuse :: (DiffusionGrid, DiffusionRules) -> (DiffusionGrid, DiffusionRules)
diffuse (dg, dr) = (aimap dg (applyRules dg dr), updateRules dr)
