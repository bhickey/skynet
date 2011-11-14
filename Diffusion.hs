{-# LANGUAGE FlexibleContexts #-}
module Diffusion where
          
import Control.DeepSeq
import Control.Monad.Primitive                     
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Point
import Ants
import Neighbors

brightness :: [Char]
brightness =  " .`-_':,;^=+/\"|)\\<>)iv%xclrs{*}I?!][1taeo7zjLu" ++ 
              "nTJCwfy325Fp6mqSghVd4EgXPGZbYkOA&8U$@KHDBWNMR0Q";

type DiffusionGrid = V.Vector Automata

data Automata = WaterAutomata |
                Automata Float Float Float Int Int (Maybe Owner) deriving (Eq)

instance NFData Automata where
 rnf WaterAutomata = ()
 rnf (Automata a b c d e f) = 
  rnf a `seq` rnf b `seq`
  rnf c `seq` rnf d `seq`
  rnf e `seq` rnf f `seq` ()

instance Ord Automata where
  compare WaterAutomata _ = LT
  compare _ WaterAutomata = GT
  compare a b = 
    let hill = compare (enemyHill a) (enemyHill b)
        f    = compare (foodProb a) (foodProb b)
        myHill = compare (friendlyHill b) (friendlyHill a) in
      firstNEQ [hill, f, myHill,LT]
      where firstNEQ = head . filter (\ x -> x /= EQ) 

instance Show Automata where
  show WaterAutomata = "#"
  show (Automata _ _ _ _ _ _) = " "
    --if f > 100.0 - (length brightness) 
    --then [brightness !! (length brightness - (100 - f) -1)]
    --else " "

friendlyAnt :: Automata -> Float
friendlyAnt WaterAutomata = 0
friendlyAnt (Automata f _ _ _ _ _) = f

enemyAnt :: Automata -> Float
enemyAnt WaterAutomata = 0
enemyAnt (Automata _ e _ _ _ _) = e

foodProb :: Automata -> Float
foodProb WaterAutomata = 0
foodProb (Automata _ _ fp _ _ _) = fp

friendlyHill :: Automata -> Int
friendlyHill WaterAutomata = 0
friendlyHill (Automata _ _ _ fh _ _) = fh

enemyHill :: Automata -> Int
enemyHill WaterAutomata = 0
enemyHill (Automata _ _ _ _ eh _) = eh

reachableBy :: Automata -> Maybe Owner
reachableBy WaterAutomata = Nothing
reachableBy (Automata _ _ _ _ _ x) = x

emptyAutomata :: Automata
emptyAutomata = Automata 0 0 0 0 0 Nothing

waterAutomata :: Automata
waterAutomata = WaterAutomata

itemToEnum :: Item -> Automata
itemToEnum (LiveAntItem Me)  = Automata 1.0 0 0 0 0 (Just Me)
itemToEnum (LiveAntItem x) = Automata 0 1.0 0 0 0 (Just x)
itemToEnum (DeadAntItem _)   = emptyAutomata
itemToEnum (HillItem Me)     = Automata 0 0 0 100 0 Nothing
itemToEnum (HillItem _)      = Automata 0 0 0 0 100 Nothing
itemToEnum FoodItem          = Automata 0 0 1.0 0 0 Nothing
itemToEnum BlankItem         = emptyAutomata

tileToEnum :: Tile -> Automata
tileToEnum WaterTile       = waterAutomata
tileToEnum (LandTile item) = itemToEnum item
tileToEnum UnknownTile     = emptyAutomata

type Rule a = a -> Neighbors a -> a

testRule :: Rule Automata
testRule WaterAutomata _ = WaterAutomata
testRule (Automata a e fD h eh r) tiles =
  let a'  = max a (F.foldl (\ f n -> f + (friendlyAnt n)/4) a tiles) / 2
      e'  = max e (F.foldl (\ f n -> f + (enemyAnt n)/4) a tiles) / 2
      fD' = F.foldl (\ f n -> max f (foodProb n * (1.0 - friendlyAnt n))) fD tiles
      h'  = F.foldl (\ f n -> max f (friendlyHill n - 1)) h tiles 
      eh'  = F.foldl (\ f n -> max f (enemyHill n - 1)) eh tiles 
      r'  = if isJust r
            then r
            else case mapMaybe reachableBy $ F.toList tiles of
                   [] -> Nothing
                   hd:_ -> Just hd
      in
    Automata a' e' fD' h' eh' r'

applyRule :: (NFData e, PrimMonad m)  => V.Vector SmartPoint -> (Rule e) -> MV.MVector (PrimState m) e -> MV.MVector (PrimState m) e -> m ()
applyRule smartPoints rule grid dest = do
  V.forM_ smartPoints
        (\ p -> do let i = dumbPoint p
                   v <- MV.unsafeRead grid i
                   ns <- T.mapM (MV.unsafeRead grid . dumbPoint) (neighbors p)
                   let new = (rule v ns) 
                   seq (rnf new) $ MV.unsafeWrite dest i new)

--Look at the strictness of this
diffuse :: V.Vector SmartPoint -> ImputedWorld -> Int -> DiffusionGrid
diffuse points iw steps = 
 V.create $ do
  grid1 <- V.thaw $ V.map tileToEnum iw
  grid2 <- MV.clone grid1
  applyRules grid1 grid2 steps
  where
    applyRules g1 _  0 = return g1
    applyRules g1 g2 n = applyRule points testRule g1 g2 >> applyRules g2 g1 (n-1)

diffusionScore :: DiffusionGrid -> SmartPoint -> Direction
diffusionScore dg p = maxDirection $ fmap ((dg V.!) . dumbPoint) (neighbors p)


showGrid :: BoundingBox -> DiffusionGrid -> String
showGrid b@(mr,mc) g = 
 let points = [[show $ g V.! (point b r c) | c <- [0..(mc-1)]] | r <- [0..(mr-1)]] in
     unlines $ map unwords points

