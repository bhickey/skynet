{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Point(
 Point(),
 row,col,
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

) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Applicative
import GameParams
import Control.DeepSeq


data Direction = North | East | South | West deriving (Bounded, Eq, Enum, Ord)

instance NFData Direction where
 rnf North = ()
 rnf East = ()
 rnf South = ()
 rnf West = ()

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

directions :: Neighbors Direction
directions = Neighbors North East South West

data Neighbors a = Neighbors !a !a !a !a
instance Functor Neighbors where
 fmap f (Neighbors a b c d) = Neighbors (f a) (f b) (f c) (f d)

instance Applicative Neighbors where
 pure a = Neighbors a a a a
 (Neighbors fa fb fc fd) <*> (Neighbors a b c d) =
   Neighbors (fa a) (fb b) (fc c) (fd d)
 _ *> a = a
 a <* _ = a

instance F.Foldable Neighbors where
 foldr f base (Neighbors a b c d) =
  F.foldr f base [a,b,c,d]

instance T.Traversable Neighbors where
 traverse f (Neighbors a b c d) =
  pure combine <*> T.traverse f [a,b,c,d]
  where combine [w,x,y,z] = Neighbors w x y z
        combine _ = undefined

neighbors :: GameParams -> Point -> Neighbors Point
neighbors gp p = fmap (neighbor gp p) directions

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

type Point = Int 

row :: GameParams -> Point -> Int
row gp s = s `div` (cols gp)
col :: GameParams -> Point -> Int
col gp s = s `mod` (cols gp)

point :: GameParams -> Int -> Int -> Point
point gp r c = (r `mod` mR) * mC + (c `mod` mC)
 where
  mR = rows gp
  mC = cols gp

neighbor :: GameParams -> Point -> Direction -> Point
neighbor gp p North = deltaPoint gp 0 1 p
neighbor gp p East = deltaPoint gp 1 0 p
neighbor gp p South = deltaPoint gp 0 (-1) p
neighbor gp p West = deltaPoint gp (-1) 0 p
    
deltaPoint :: GameParams -> Int -> Int -> Point -> Point
deltaPoint gp x y s =
 (s + (x * mc) + y) `mod` (mr * mc)
 where
  mr = rows gp
  mc = cols gp

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
distance :: GameParams -> Point -> Point -> Int
distance gp p1 p2 =
  let rowd = modDistance r (row gp p1) (row gp p2)
      cold = modDistance c (col gp p1) (col gp p2)
  in rowd + cold
  where
   r = cols gp 
   c = cols gp 


-- | Computes the square of the two norm.
twoNormSquared :: (Int, Int) -> Int
twoNormSquared (r,c) = r ^ (2::Int) + c ^ (2::Int)

getPointCircle :: GameParams -> Int -- radius squared
               -> Point -> [Point]
getPointCircle gp r2 p =
  let rx = truncate.sqrt.(fromIntegral::Int -> Double) $ r2
  in map deltaPoint' $ filter ((<=r2).twoNormSquared) $ (,) <$> [-rx..rx] <*> [-rx..rx]
  where deltaPoint' (x,y) = deltaPoint gp x y p 

viewCircle :: GameParams -> Point -> [Point]
viewCircle gp p = getPointCircle gp (viewradius2 gp) p

--
{-
showGrid :: (Show a) => Array Point a -> String
showGrid g = 
  let (Point s1, Point s2) = bounds g
      points = [[show $ g ! (Point r c) | c <- [c0..c1]] | r <- [r0..r1]] in
    unlines $ map unwords points
-}
