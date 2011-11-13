module Ants
  (
    -- Data structures
    Owner (..)
  , Ant (..)
  , Direction (..)
  , GameState (..)
  , Order (Order)
  , World
  , ImputedWorld
  , Hill (..)
  , Item (..)
  , Tile (..)
  , MetaTile (..)
  , GameParams (..)
  , Visibility (..)

    -- Tile Functions
  , isLiveAnt
  , isDeadAnt
  , isLiveEnemyAnt
  , isDeadEnemyAnt
  , isHill
  , isEnemyHill
  , isFood
  , isWater
  , visibleMetaTile
  , toOwner
  , impute
    -- Utility functions
  , myAnts
  , enemyAnts
  , passable
  , distance
  , neighbors
  
    -- Debugging
  --, renderWorld
  ,renderMetaTile

  ) where


import Data.Array
import qualified Data.Array.IArray as IA
import Data.Char (toUpper)

import Data.Time.Clock

import Point
import Control.DeepSeq
import GameParams

--------------------------------------------------------------------------------
-- Tiles -----------------------------------------------------------------------
--------------------------------------------------------------------------------
data Item = LiveAntItem Owner | DeadAntItem Owner | HillItem Owner | FoodItem | BlankItem deriving (Show,Eq)
data Tile = LandTile Item | WaterTile | UnknownTile deriving (Show,Eq)

data Visibility = 
    Observed
  | Unobserved
  | Predicted
  deriving (Show, Eq)

-- | Elements of the world
data MetaTile = MetaTile
  { tile :: !Tile
  , visible :: !Visibility
  } deriving (Show)

isLiveAnt, isDeadAnt, isLiveEnemyAnt, isDeadEnemyAnt, isHill, isEnemyHill, isFood, isWater :: Tile -> Bool

isLiveAnt (LandTile (LiveAntItem _)) = True
isLiveAnt _ = False

isDeadAnt (LandTile (DeadAntItem _)) = True
isDeadAnt _ = False

isLiveEnemyAnt (LandTile (LiveAntItem (Enemy _))) = True
isLiveEnemyAnt _ = False

isDeadEnemyAnt (LandTile (DeadAntItem (Enemy _))) = True
isDeadEnemyAnt _ = False

isHill (LandTile (HillItem _)) = True
isHill _ = False

isEnemyHill (LandTile (HillItem (Enemy _))) = True
isEnemyHill _ = False

isWater WaterTile = True
isWater _ = False

isFood (LandTile FoodItem) = True
isFood _ = False



-- | For debugging

renderItem :: Item -> Char
renderItem (LiveAntItem Me) = 'm'
renderItem (LiveAntItem _) = 'e'
renderItem (DeadAntItem _) = 'd'
renderItem (HillItem Me) = 'h'
renderItem (HillItem _) = 'x'
renderItem BlankItem = 'l'
renderItem FoodItem = 'f'

renderTile :: Tile -> Char
renderTile (LandTile item) = renderItem item
renderTile WaterTile = 'w'
renderTile UnknownTile = '*'

renderMetaTile :: MetaTile -> Char
renderMetaTile (MetaTile t v) =
  upper $ renderTile t
  where
    upper :: Char -> Char
    upper =
      case v of
        Observed -> toUpper
        Unobserved -> id
        Predicted -> id


-- | Sets the tile to visible, if the tile is still unknown then it is land.
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile (MetaTile UnknownTile _) = MetaTile (LandTile BlankItem) Observed
visibleMetaTile (MetaTile t _) = MetaTile t Observed


--------------------------------------------------------------------------------
-- Immutable World -------------------------------------------------------------
--------------------------------------------------------------------------------
type World = Array Point MetaTile
type ImputedWorld = Array Point Tile

impute :: World -> ImputedWorld
impute w = IA.amap tile w

{-
colBound :: World -> Int
colBound = col.snd.bounds


-- | For debugging
renderWorld :: World -> String
renderWorld w = concatMap renderAssoc (assocs w)
  where
    maxColumn = colBound w
    renderAssoc :: (Point, MetaTile) -> String
    renderAssoc a
      | col (fst a) == maxColumn = [renderMetaTile (snd a)] ++ "\n"
      | otherwise = [renderMetaTile (snd a)]
-}




--------------------------------------------------------------------------------
-- Ants ------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Owner = Me | Enemy !Int deriving (Show,Eq)

instance NFData Owner where
 rnf Me = ()
 rnf (Enemy x) = seq x ()

data Ant = Ant
  { pointAnt :: !Point
  , ownerAnt :: !Owner
  } deriving (Show)

instance NFData Ant where
 rnf (Ant p o) = rnf p `seq` rnf o `seq` ()


isMe, isEnemy :: Ant -> Bool
isMe = (==Me).ownerAnt
isEnemy = not.isMe

myAnts, enemyAnts :: [Ant] -> [Ant]
myAnts = filter isMe
enemyAnts = filter isEnemy

--------------------------------------------------------------------------------
-- Hills -----------------------------------------------------------------------
--------------------------------------------------------------------------------
data Hill = Hill
  { pointHill :: !Point
  , ownerHill :: !Owner
  } deriving (Show)

{-
isMy, isEnemy's :: Hill -> Bool
isMy = (==Me).ownerHill
isEnemy's = not.isMy

myHills, enemyHills :: [Hill] -> [Hill]
myHills = filter isMy
enemyHills = filter isEnemy's
-}

--------------------------------------------------------------------------------
-- Orders ----------------------------------------------------------------------
--------------------------------------------------------------------------------


data Order = Order Ant Direction deriving (Show)

instance NFData Order where
 rnf (Order a d) = rnf a `seq` rnf d `seq` ()

passable :: GameParams -> World -> Order -> Bool
passable gp w (Order ant direction) =
  let newPoint = neighbor gp (pointAnt ant) direction
  in  isWater $ tile (w ! newPoint)

toOwner :: Int -> Owner
toOwner 0 = Me
toOwner a = Enemy a

--------------------------------------------------------------------------------
-- GameDetails -----------------------------------------------------------------
--------------------------------------------------------------------------------
type Food = Point

data GameState = GameState
  { world :: World
  , ants :: [Ant] -- call "ants GameState" to all ants
  , food :: [Food] -- call "food GameState" to all food
  , hills :: [Hill] -- call "hills GameState" to all hills
  , startTime :: UTCTime
  }

