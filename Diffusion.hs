module Diffusion where
                  
import Data.Array.Diff
import Data.Ix
import Point
import Data.Maybe
import Ants

aimap :: (Ix i) => (Array i e) -> (i -> e -> c) -> (Array i c)
aimap a f = listArray (bounds a) $ fmap (uncurry f) (assocs a)

data Automata = FriendlyAnt | EnemyAnt | Food | MyHill | EnemyHill | WaterTile deriving (Bounded, Eq, Enum, Ix, Ord)

emptyAutomata = array (FriendlyAnt, EnemyHill) $ zip [FriendlyAnt .. WaterTile] (repeat 0.0)

tileToEnum :: Tile -> AutomataArray
tileToEnum (AntTile Me)  = emptyAutomata // [(FriendlyAnt, 1.0)]
tileToEnum (AntTile _)   = emptyAutomata // [(EnemyAnt, 1.0)]
tileToEnum (HillTile Me) = emptyAutomata // [(MyHill, 1.0)]
tileToEnum (HillTile _)  = emptyAutomata // [(EnemyAnt, 1.0)]
tileToEnum FoodTile  = emptyAutomata // [(Food, 1.0)]
tileToEnum Water = emptyAutomata // [(WaterTile, 1.0)]
tileToEnum _ = emptyAutomata

type AutomataArray = Array Automata Float
type DiffusionGrid = Array Point AutomataArray
type Rule = (AutomataArray -> Float -> Float -> Float)
type DiffusionRules = Array Automata (Maybe (Int, Rule))

rules :: DiffusionRules
rules = array (FriendlyAnt, WaterTile) $
    [(FriendlyAnt, Just (5, friendRule)),
     (EnemyAnt, Just (5, enemyRule)),
     (Food, Just (20, foodRule)),
     (MyHill, Just (20, hillRule)),
     (EnemyHill, Just (20, enemyHillRule)),
     (WaterTile, Just (20, waterRule))]

friendRule :: Rule
friendRule aa x y =
   if aa ! WaterTile == 1.0
   then 0.0
   else max x y

enemyRule = friendRule
foodRule = friendRule
hillRule = friendRule
enemyHillRule = friendRule
waterRule = friendRule

updateRule :: Maybe (Int, Rule) -> Maybe (Int, Rule)
updateRule (Just (1, _)) = Nothing
updateRule (Just (x, f)) = Just (x - 1, f)
updateRule Nothing = Nothing

updateRules :: DiffusionRules -> DiffusionRules
updateRules = amap updateRule

diffusionGrid :: ImputedWorld -> DiffusionGrid
diffusionGrid w = amap tileToEnum w

getRule :: DiffusionRules -> Automata -> Rule
getRule dr auto = 
  case dr ! auto of
    Nothing -> (\ _ x _ -> x)
    Just (_, f) -> f

mapRules :: DiffusionRules -> AutomataArray -> AutomataArray -> AutomataArray
mapRules dr a b = aimap a (\ i e -> ((getRule dr i) a) e (b ! i))

applyRules :: DiffusionGrid -> DiffusionRules -> Point -> AutomataArray -> AutomataArray
applyRules grid rules i e =
  let n = (grid ! i):(map (grid !) (neighbors grid i)) in
   foldl1 (mapRules rules) n
    
diffuse :: (DiffusionGrid, DiffusionRules) -> (DiffusionGrid, DiffusionRules)
diffuse (dg, dr) = (aimap dg (applyRules dg dr), updateRules dr)
