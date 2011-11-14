module Ants
  (
    -- Data structures
    Owner (..)
  , Ant (..)
  , Direction (..)
  , GameState (..)
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
  
    -- Debugging
  --, renderWorld
  ,renderMetaTile

  ) where


import qualified Data.Vector as V
import Data.Char (toUpper)

import Data.Time.Clock

import Point
import Neighbors
import GameParams
import Control.DeepSeq

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
type World = V.Vector MetaTile
type ImputedWorld = V.Vector Tile

impute :: World -> ImputedWorld
impute w = V.map tile w

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
data Owner = Me | Enemy !Int deriving (Show, Eq, Ord)

instance NFData Owner where
 rnf Me = ()
 rnf (Enemy x) = seq x ()

data Ant = Ant
  { pointAnt :: !SmartPoint
  , ownerAnt :: !Owner
  } deriving (Show, Eq, Ord)


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
  { pointHill :: !SmartPoint
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

toOwner :: Int -> Owner
toOwner 0 = Me
toOwner a = Enemy a

--------------------------------------------------------------------------------
-- GameDetails -----------------------------------------------------------------
--------------------------------------------------------------------------------
type Food = SmartPoint

data GameState = GameState
  { world :: World
  , ants :: [Ant] -- call "ants GameState" to all ants
  , food :: [Food] -- call "food GameState" to all food
  , hills :: [Hill] -- call "hills GameState" to all hills
  , startTime :: UTCTime
  }

