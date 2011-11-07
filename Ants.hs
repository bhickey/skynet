module Ants
  (
    -- Data structures
    Owner (..)
  , Ant (..)
  , Direction (..)
  , GameState (..)
  , Order (..)
  , World
  , ImputedWorld
  , Hill (..)
  , Tile (..)
  , MetaTile (..)
  , GameParams (..)
  , Visibility (..)

    -- Coordinates
  , Point
  , Row
  , Col
  , (%!)

    -- Tile Functions
  , isAnt
  , isDead
  , isWater
  , getPointCircle
  , visibleMetaTile
  , toOwner

    -- Utility functions
  , myAnts
  , enemyAnts
  , passable
  , distance
  , neighbors

  ) where

import Control.Applicative
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt, toUpper)
import Data.Maybe

import Data.Time.Clock
import System.IO

import Point
import Util

--------------------------------------------------------------------------------
-- Tiles -----------------------------------------------------------------------
--------------------------------------------------------------------------------
data Tile = AntTile Owner
          | Dead Owner
          | HillTile Owner
          | Land
          | FoodTile
          | Water
          | Unknown
          deriving (Show,Eq)

data Visibility = 
    Observed
  | Unobserved
  | Predicted
  deriving (Show, Eq)

-- | Elements of the world
data MetaTile = MetaTile
  { tile :: Tile
  , visible :: Visibility
  } deriving (Show)

isAnt, isDead, isAntEnemy, isDeadEnemy :: Tile -> Bool
isAnt (AntTile _) = True
isAnt _ = False

isDead (Dead _) = True
isDead _ = False

isHill (HillTile _) = True
isHill _ = False

isAntEnemy (AntTile (Enemy _)) = True
isAntEnemy _ = False

isHillEnemy (HillTile (Enemy _)) = True
isHillEnemy _ = False

isDeadEnemy (Dead (Enemy _)) = True
isDeadEnemy _ = False

-- | For debugging
renderTile :: MetaTile -> String
renderTile m
  | tile m == AntTile Me = visibleUpper m 'm'
  | isAntEnemy $ tile m = visibleUpper m 'e'
  | tile m == Dead Me = visibleUpper m 'd'
  | isDeadEnemy $ tile m = visibleUpper m 'd'
  | tile m == Land = visibleUpper m 'l'
  | tile m == HillTile Me = visibleUpper m 'h'
  | isHillEnemy $ tile m = visibleUpper m 'x'
  | tile m == FoodTile = visibleUpper m 'f'
  | tile m == Water = visibleUpper m 'w'
  | otherwise = "*"
  where
    visibleUpper :: MetaTile -> Char -> String
    visibleUpper mt c =
      case visible mt of
        Observed -> [toUpper c]
        _ -> [c]

-- | Sets the tile to visible, if the tile is still unknown then it is land.
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m
  | tile m == Unknown = MetaTile {tile = Land, visible = Observed }
  | otherwise         = MetaTile {tile = tile m, visible = Observed }


--------------------------------------------------------------------------------
-- Immutable World -------------------------------------------------------------
--------------------------------------------------------------------------------
type World = Array Point MetaTile
type ImputedWorld = Array Point Tile

colBound :: World -> Col
colBound = col.snd.bounds

rowBound :: World -> Row
rowBound = row.snd.bounds

-- | Accesses World using the modulus of the point
(%!) :: World -> Point -> MetaTile
(%!) w p = w ! p

-- | For debugging
renderWorld :: World -> String
renderWorld w = concatMap renderAssoc (assocs w)
  where
    maxCol = colBound w
    renderAssoc :: (Point, MetaTile) -> String
    renderAssoc a
      | col (fst a) == maxCol = renderTile (snd a) ++ "\n"
      | otherwise = renderTile (snd a)

--------------------------------------------------------------------------------
-- Norms and Metrics -----------------------------------------------------------
-- https://secure.wikimedia.org/wikipedia/en/wiki/Norm_(mathematics) -----------
--------------------------------------------------------------------------------
modDistance :: Int -- modulus
            -> Int -> Int -> Int
modDistance m x y = 
  let a = abs $ x - y
  in min a (m - a)

-- | Computes manhattan distance.
distance :: Point -> Point -> Int
distance p1 p2 =
  let rowd = modDistance (maxRow p1) (row p1) (row p2)
      cold = modDistance (maxCol p1) (col p1) (col p2)
  in rowd + cold

-- | Computes the square of the two norm.
twoNormSquared :: (Row, Col) -> Int
twoNormSquared (r,c) = r ^ (2::Int) + c ^ (2::Int)

inCycle :: Int -> Int -> Int
inCycle c x =
  if x < 0
  then (x + c) `mod` c
  else x `mod` c

isWater :: World -> Point -> Bool
isWater w p = tile (w ! p) == Water

neighbors :: (Array Point e) -> Point -> [Point]
neighbors w p =
    let n = neighbor p in
        [(n East)
        ,(n West)
        ,(n South)
        ,(n North)
        ]


getPointCircle :: Int -- radius squared
               -> (Row, Col) -> [Point]
getPointCircle r2 (mr,mc) =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in map (\ (r,c) -> Point r c mr mc) $ filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]

--------------------------------------------------------------------------------
-- Ants ------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Owner = Me | Enemy Int deriving (Show,Eq)

data Ant = Ant
  { pointAnt :: Point
  , ownerAnt :: Owner
  } deriving (Show)

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
  { pointHill :: Point
  , ownerHill :: Owner
  } deriving (Show)

isMy, isEnemy's :: Hill -> Bool
isMy = (==Me).ownerHill
isEnemy's = not.isMy

myHills, enemyHills :: [Hill] -> [Hill]
myHills = filter isMy
enemyHills = filter isEnemy's

--------------------------------------------------------------------------------
-- Orders ----------------------------------------------------------------------
--------------------------------------------------------------------------------


instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

data Order = Order
  { ant :: Ant
  , direction :: Direction
  } deriving (Show)

move :: Direction -> Point -> Point
move dir (Point r c mr mc)
  | dir == North = Point (r - 1) c mr mc
  | dir == South = Point (r + 1) c mr mc
  | dir == West  = Point r (c - 1) mr mc
  | dir == East  = Point r (c + 1) mr mc

passable :: World -> Order -> Bool
passable w order =
  let newPoint = move (direction order) (pointAnt $ ant order)
  in  tile (w %! newPoint) /= Water

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

data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , playerSeed :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int
  , spawnradius2 :: Int
  , viewCircle :: [Point]
  , attackCircle :: [Point]
  , spawnCircle :: [Point]
  } deriving (Show)

