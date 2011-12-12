module Neighbors (
 Direction(..),
 Neighbors(..),
 directions,
 maxDirectionValue,
 minDirectionValue,
 maxDirection,
 minDirection,
 selectDirection,
 withDirections,
 fromDirection,
 neighborList
) where
import Control.Applicative

import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Direction = North | East | South | West deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

directions :: Neighbors Direction
directions = Neighbors North East South West
selectDirection :: Neighbors a -> Direction -> a
selectDirection (Neighbors x _ _ _) North = x
selectDirection (Neighbors _ x _ _) East  = x
selectDirection (Neighbors _ _ x _) South = x
selectDirection (Neighbors _ _ _ x) West  = x

fromDirection :: Direction -> Direction
fromDirection North = South
fromDirection East = West
fromDirection South = North
fromDirection West = East

data Neighbors a = Neighbors !a !a !a !a deriving (Show)
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

withDirections :: Neighbors a -> Neighbors (Direction, a)
withDirections n = pure (,) <*> directions <*> n

maxDirectionValue :: Ord a => Neighbors a -> (Direction, a)
maxDirectionValue n =
 F.maximumBy compareSnd $ withDirections n
 where compareSnd (_,a) (_,b) = compare a b

minDirectionValue :: Ord a => Neighbors a -> (Direction, a)
minDirectionValue n =
 F.minimumBy compareSnd $ withDirections n
 where compareSnd (_,a) (_,b) = compare a b

maxDirection :: Ord a => Neighbors a -> Direction
maxDirection = fst.maxDirectionValue
minDirection :: Ord a => Neighbors a -> Direction
minDirection = fst.minDirectionValue

neighborList :: Neighbors a -> [(Direction, a)]
neighborList (Neighbors n e s w) = zip [North,East,South,West] [n,e,s,w]
