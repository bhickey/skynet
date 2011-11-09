module Point where

import Data.Ix

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
  , maxRow :: Row
  , maxCol :: Col
  } deriving (Show, Eq, Ord)

-- We should probably be checking these values
instance Ix Point where
  range (Point ra ca mra mca, Point rb cb mrb mcb) = [Point r c mra mca | r <- [ra..rb], c <- [ca..cb]]
  index (a,b) p = (row p - row a) * (maxCol p - col a) +  (col p - col a)
  inRange (a,b) i =
    (row a <= row i && row i <= row b) &&
    (col a <= col i && col i <= col b)

neighbor :: Point -> Direction -> Point
neighbor (Point r c mR mC) North = (Point ((r + 1) `mod` mR) c mR mC)
neighbor (Point r c mR mC) East = (Point r ((c + 1) `mod` mC) mR mC)
neighbor (Point r c mR mC) South = (Point ((r - 1 + mR) `mod` mR) c mR mC)
neighbor (Point r c mR mC) West = (Point r ((c - 1 + mC) `mod` mC) mR mC)
    
