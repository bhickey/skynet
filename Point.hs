module Point(
 Point(row,col),
 Direction(..),
 directions,
 neighbor,
 point,
 viewCircle,
 distance,
 

) where

import Data.Ix
import Control.Applicative
import GameParams

data Direction = North | East | South | West deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

directions :: [Direction]
directions = [North .. West]

type Row = Int
type Col = Int
data Point = Point
  { row :: Row
  , col :: Col
  , _maxRow :: Row
  , _maxCol :: Col
  } deriving (Show, Eq, Ord)

-- We should probably be checking these values
instance Ix Point where
  range (Point ra ca mra mca, Point rb cb _mrb _mcb) = [Point r c mra mca | r <- [ra..rb], c <- [ca..cb]]
  index (Point ra ca _mra _mca, _b) (Point rp cp _mrp mcp) =
    (rp - ra) * (mcp - ca) + (cp - ca)
  inRange (a,b) i =
    (row a <= row i && row i <= row b) &&
    (col a <= col i && col i <= col b)


point :: GameParams -> Int -> Int -> Point
point gp r c = Point (r `mod` mR) (c `mod` mC) mR mC
 where
  mR = rows gp
  mC = cols gp

neighbor :: Point -> Direction -> Point
neighbor (Point r c mR mC) North = (Point ((r + 1) `mod` mR) c mR mC)
neighbor (Point r c mR mC) East = (Point r ((c + 1) `mod` mC) mR mC)
neighbor (Point r c mR mC) South = (Point ((r - 1 + mR) `mod` mR) c mR mC)
neighbor (Point r c mR mC) West = (Point r ((c - 1 + mC) `mod` mC) mR mC)
    


deltaPoint :: Int -> Int -> Point -> Point
deltaPoint x y (Point r c mr mc) =
 Point ((x + r) `mod` mr) ((y + c) `mod` mc) mr mc

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
distance (Point r1 c1 mr1 mc1) (Point r2 c2 _mr2 _mc2) =
  let rowd = modDistance mr1 r1 r2
      cold = modDistance mc1 c1 c2
  in rowd + cold



-- | Computes the square of the two norm.
twoNormSquared :: (Row, Col) -> Int
twoNormSquared (r,c) = r ^ (2::Int) + c ^ (2::Int)


getPointCircle :: Int -- radius squared
               -> Point -> [Point]
getPointCircle r2 p =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in map deltaPoint' $ filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]
  where deltaPoint' (x,y) = deltaPoint x y p 

viewCircle :: GameParams -> Point -> [Point]
viewCircle gp p = getPointCircle (viewradius2 gp) p

