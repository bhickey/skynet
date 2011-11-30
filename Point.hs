{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Point(
 Point(),
 BoundingBox,
 row,col,
 SmartPoint,
 SmartGrid,
 SmartVector,
 smartWorld,
 dumbPoint,
 neighbors,
 neighbor,
 point,
 getPointCircle,
) where

import qualified Data.Vector as V
import Neighbors
import Control.Applicative

type BoundingBox = (Int,Int)

boundingBoxSize :: (Int,Int) -> Int
boundingBoxSize = uncurry (*)

type Point = Int 

row :: BoundingBox -> Point -> Int
row (_,c) s = s `div` c
col :: BoundingBox -> Point -> Int
col (_,c) s = s `mod` c 

point :: BoundingBox -> Int -> Int -> Point
point (mR,mC) r c = (r `mod` mR) * mC + (c `mod` mC)

data SmartPoint = SmartPoint (Neighbors SmartPoint) Point

instance Eq SmartPoint where
 s1 == s2 = dumbPoint s1 == dumbPoint s2

instance Ord SmartPoint where
 compare s1 s2 = compare (dumbPoint s1) (dumbPoint s2)

instance Show SmartPoint where
 show s = "SmartPoint " ++ (show $ dumbPoint s)


dumbPoint :: SmartPoint -> Point
dumbPoint (SmartPoint _ p) = p

neighbors :: SmartPoint -> (Neighbors SmartPoint)
neighbors (SmartPoint n _) = n

neighbor :: SmartPoint -> Direction -> SmartPoint
neighbor sp d = selectDirection (neighbors sp) d
 
-- Note for correct sharing the first argument should only be
-- provided once
type SmartGrid = Int -> Int -> SmartPoint
type SmartVector = V.Vector SmartPoint
smartWorld :: BoundingBox -> (SmartGrid,SmartVector)
smartWorld box = (find,storage)
  where
   find r c = storage V.! point box r c
   storage = V.generate (boundingBoxSize box)  construct
   construct s = SmartPoint (fmap (getNeighbor s) directions) s 
   getNeighbor s dir = storage V.! dumbNeighbor s dir
   dumbNeighbor :: Point -> Direction -> Point
   dumbNeighbor p North = deltaPoint box (-1) 0 p
   dumbNeighbor p East = deltaPoint box 0 1 p
   dumbNeighbor p South = deltaPoint box 1 0 p
   dumbNeighbor p West = deltaPoint box 0 (-1) p


    
deltaPoint :: BoundingBox -> Int -> Int -> Point -> Point
deltaPoint (mr,mc) r c s =
 let oldr = div s mc
     oldc = mod s mc
     newr = (oldr + r + mr) `mod` mr
     newc = (oldc + c + mc) `mod` mc in
   newr * mc + newc

deltaSmartPoint :: Int -> Int -> SmartPoint -> SmartPoint
deltaSmartPoint r c s = moveNS (moveEW s c) r
 where
  moveNS p v = move p (if v < 0 then North else South) (abs v)
  moveEW p v = move p (if v < 0 then West else East) (abs v)
  move p _ 0 = p 
  move p d v = move (neighbor p d) d (v-1)



--------------------------------------------------------------------------------
-- Norms and Metrics -----------------------------------------------------------
-- https://secure.wikimedia.org/wikipedia/en/wiki/Norm_(mathematics) -----------
--------------------------------------------------------------------------------

-- | Computes the square of the two norm.
twoNormSquared :: (Int, Int) -> Int
twoNormSquared (r,c) = r ^ (2::Int) + c ^ (2::Int)

getPointCircle :: Int -- radius squared
                  -> SmartPoint -> [SmartPoint]
getPointCircle r2 p =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in map deltaPoint' $ filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]
  where deltaPoint' (r,c) = deltaSmartPoint r c p 


