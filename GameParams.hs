module GameParams (
  GameParams(..),
  viewCircle,
  attackCircle,
  spawnCircle,
) where

import Point

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
  , smartGrid :: SmartGrid
  , smartVector :: SmartVector
  } 

viewCircle :: GameParams -> SmartPoint -> [SmartPoint]
viewCircle gp p = getPointCircle (viewradius2 gp) p
attackCircle :: GameParams -> SmartPoint -> [SmartPoint]
attackCircle gp p = getPointCircle (attackradius2 gp) p
spawnCircle :: GameParams -> SmartPoint -> [SmartPoint]
spawnCircle gp p = getPointCircle (spawnradius2 gp) p
