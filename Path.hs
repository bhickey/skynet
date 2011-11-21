module Path (
  explore,
  spiral1,
  spiral2,
  hilbert1,
  hilbert2,
  peano1,
  mirror
) where

import Neighbors
import Point

explore :: [Direction] -> (SmartPoint -> Bool) -> SmartPoint -> Maybe SmartPoint
explore [] f p = if f p then Just p else Nothing
explore (h:t) f p = if f p then Just p else explore t f (neighbor p h)
      
mirror :: [Direction] -> [Direction]
mirror d = map reverse d where 
  reverse North = South
  reverse East = West
  reverse South = North
  reverse West = East

rotate :: [Direction] -> [Direction]
rotate d = map rot d where
  rot North = East
  rot East = South
  rot South = West
  rot West = North

spiral1 :: [Direction]
spiral1 = [South, East, North, North, West, West, South, South, South]
spiral2 :: [Direction]
spiral2 = spiral1 ++ [South, 
                      East, East, East, 
                      North, North, North, North, 
                      West, West, West, West, 
                      South, South, South, South]

hilbert1 :: [Direction]
hilbert1 = [South, East, North]
hilbert2 :: [Direction]
hilbert2 = [East, South, West, 
            South, South, East, North, 
            East, South, East, North, 
            North, West, North, East]

peano1 :: [Direction]
peano1 = [North, North, East, South, South, East, North, North]
