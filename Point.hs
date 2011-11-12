module Point(
 Point(row,col),
 Direction(..),
 Neighbors,
 directions,
 neighbor,
 neighbors,
 point,
 viewCircle,
 distance,
 maxDirectionValue,
 minDirectionValue,
 maxDirection,
 minDirection,

 showGrid
) where

import Data.Ix
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Applicative
import GameParams

import Data.Array

data Direction = North | East | South | West deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

directions :: Neighbors Direction
directions = Neighbors (North,East,South,West)

newtype Neighbors a = Neighbors (a,a,a,a)
instance Functor Neighbors where
 fmap f (Neighbors (a,b,c,d)) = Neighbors (f a,f b,f c,f d)

instance Applicative Neighbors where
 pure a = Neighbors (a, a, a, a)
 Neighbors (fa,fb,fc,fd) <*> Neighbors (a,b,c,d) =
   Neighbors (fa a, fb b, fc c, fd d)
 _ *> a = a
 a <* _ = a

instance F.Foldable Neighbors where
 foldr f base (Neighbors (a,b,c,d)) =
  F.foldr f base [a,b,c,d]

instance T.Traversable Neighbors where
 traverse f (Neighbors (a,b,c,d)) =
  pure combine <*> T.traverse f [a,b,c,d]
  where combine [w,x,y,z] = Neighbors (w,x,y,z)
        combine _ = undefined

neighbors :: Point -> Neighbors Point
neighbors p = fmap (neighbor p) directions

maxDirectionValue :: Ord a => Neighbors a -> (Direction, a)
maxDirectionValue n =
 F.maximumBy compareSnd $ pure (,) <*> directions <*> n
 where compareSnd (_,a) (_,b) = compare a b
minDirectionValue :: Ord a => Neighbors a -> (Direction, a)
minDirectionValue n =
 F.minimumBy compareSnd $ pure (,) <*> directions <*> n
 where compareSnd (_,a) (_,b) = compare a b

maxDirection :: Ord a => Neighbors a -> Direction
maxDirection = fst.maxDirectionValue
minDirection :: Ord a => Neighbors a -> Direction
minDirection = fst.minDirectionValue

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
  index (Point ra ca _ _, Point _ cb _ _) (Point rp cp _ _) =
    (rp - ra) * (cb - ca) + (cp - ca)
  inRange (Point ra ca _ _, Point rb cb _ _) (Point rp cp _ _) =
    (ra <= rp && rp <= rb) && (ca <= cp && cp <= cb)

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

--
showGrid :: (Show a) => Array Point a -> String
showGrid g = 
  let (Point r0 c0 mr mc, Point r1 c1 _ _) = bounds g
      points = [[show $ g ! (Point r c mr mc) | c <- [c0..c1]] | r <- [r0..r1]] in
    unlines $ map unwords points
