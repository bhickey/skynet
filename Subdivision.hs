module Subdivision where

import Ants

import Data.Array
import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Random.Class
import Data.Maybe
import qualified Data.Queue as Q
import qualified Data.Set as S
import qualified Data.Map as M

type SectorId = Int
type OwnerGraph = Array Point (Maybe SectorId)

data Subdivision = Subdivision
  { divDict :: M.Map SectorId Sector
  , pointDict :: Array Point (Maybe Sector)
  }

data SearchNode = SearchNode
  { nodePoint :: Point
  , nodeId :: SectorId }

data Sector = Sector
  { subId :: SectorId
  , centroid :: Point
  , members :: S.Set Point
  , neighbors :: S.Set SectorId
  }

emptySubdivision w = Subdivision M.empty (listArray (bounds w) (repeat Nothing))

randomPoint :: (MonadRandom m) => World -> m Point
randomPoint w = do
  let ((r0,c0),(r1,c1)) = bounds w in
    do row <- getRandomR (r0, r1)
       col <- getRandomR (c0, c1)
       if isWater w (row, col)
       then return (row, col)
       else randomPoint w
            
worldSize w =
  let ((r0,c0),(r1,c1)) = bounds w in
    (r1 - r0) * (c1 - c0)

sqrtc :: Int -> Int
sqrtc x = ceiling (sqrt (fromIntegral x))

toSectors :: [Point] -> [Sector]
toSectors pl =
    zipWith (\ p id -> Sector id p (S.singleton p) S.empty) pl [0..]

subdivideWith :: [Sector] -> Subdivision -> Subdivision
subdivideWith sectors subdiv =
    let searchQueue = Q.fromList $ map (\ s -> SearchNode (centroid s) (subId s)) sectors


populateSubdivision :: World -> [Point] -> Subdivision
populateSubdivision w pl = 
  subdivideWith (toSectors pl) (emptySubdivision w)

subdivide :: (MonadRandom m) => World -> (Point -> [Point]) -> m Subdivision
subdivide w nfn = do
  points <- replicateM (sqrtc $ worldSize w) (randomPoint w)
  return $ populateSubdivision w points
