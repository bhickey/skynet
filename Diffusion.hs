{-# LANGUAGE FlexibleContexts #-}
module Diffusion where
          
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Point
import Ants

<<<<<<< HEAD
brightness :: [Char]
brightness =  " .`-_':,;^=+/\"|)\\<>)iv%xclrs{*}I?!][1taeo7zjLu" ++ 
              "nTJCwfy325Fp6mqSghVd4EgXPGZbYkOA&8U$@KHDBWNMR0Q";

type DiffusionGrid = Array Point Automata
=======
type DiffusionGrid = V.Vector Automata
>>>>>>> Moved to vector

data Automata = WaterAutomata |
                Automata Int Int Int Float Float deriving (Eq)

instance NFData Automata where
 rnf WaterAutomata = ()
 rnf (Automata a b c d e) = 
  rnf a `seq` rnf b `seq`
  rnf c `seq` rnf d `seq`
  rnf e `seq` ()

instance Ord Automata where
  compare WaterAutomata _ = LT
  compare _ WaterAutomata = GT
  compare (Automata _ _ fa _ _) (Automata _ _ fb _ _) = compare fa fb

instance Show Automata where
  show WaterAutomata = "#"
  show (Automata _ _ f _ _) = 
    if f > 100 - (length brightness) 
    then [brightness !! (length brightness - f)]
    else " "

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

type Rule a = a -> Neighbors a -> a

testRule :: Rule Automata
testRule WaterAutomata _ = WaterAutomata
testRule (Automata a _ fD _ _) tiles =
  let penalty = if a == 0 then 1 else 5
      fD' = F.foldl (\ f n -> max f (foodProb n - penalty)) fD tiles in
    Automata 0 0 fD' 0.0 0.0

applyRule :: (NFData e, PrimMonad m)  => GameParams -> (Rule e) -> MV.MVector (PrimState m) e -> MV.MVector (PrimState m) e -> m ()
applyRule gp rule grid dest = do
  let b = MV.length grid in 
   forM_ [0..(b-1)]
         (\ i -> do v <- MV.read grid i
                    ns <- T.mapM (MV.read grid) (neighbors gp i)
                    let new = (rule v ns) 
                    seq (rnf new) $ MV.write dest i new)

--Look at the strictness of this
diffuse :: GameParams -> ImputedWorld -> Int -> DiffusionGrid
diffuse gp iw steps = 
 V.create $ do
  grid1 <- V.thaw $ V.map tileToEnum iw
  grid2 <- MV.clone grid1
  applyRules grid1 grid2 steps
  where
    applyRules g1 _  0 = return g1
    applyRules g1 g2 n = applyRule gp testRule g1 g2 >> applyRules g2 g1 (n-1)

diffusionScore :: GameParams -> Array Point Automata -> Point -> Direction
diffusionScore gp dg p = minDirection $ fmap (dg !) (neighbors gp p)
