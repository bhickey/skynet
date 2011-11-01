module Subdivision where

import Ants

import Data.Array
import Data.Maybe
import System.Random
import Control.Monad
import Control.Monad.Random.Class
import qualified Data.Set as S
import qualified Data.Map as M

type SectorId = Int

data Subdivision = Subdivision
  { divDict :: M.Map SectorId Sector
  , pointDict :: Array Point (Maybe Sector)
  }

data Sector = Sector
  { subId :: SectorId
  , members :: S.Set Point
  , neighbors :: S.Set SectorId
  }

emptySubdivision w = Subdivision M.empty (listArray (bounds w) (repeat Nothing))

randomPoint :: (MonadRandom m) => World -> m Point
randomPoint w = do
  let ((r0,c0),(r1,c1)) = bounds w in
    do row <- getRandomR (r0, r1)
       col <- getRandomR (c0, c1)
       return (row, col)
            
worldSize w =
  let ((r0,c0),(r1,c1)) = bounds w in
    (r1 - r0) * (c1 - c0)

sqrtc :: Int -> Int
sqrtc x = ceiling (sqrt (fromIntegral x))

populateSubdivision :: World -> [Point] -> Subdivision
populateSubdivision w pl = emptySubdivision w

subdivide :: (MonadRandom m) => World -> (Point -> [Point]) -> m Subdivision
subdivide w nfn = do
  points <- replicateM (sqrtc $ worldSize w) (randomPoint w)
  return $ populateSubdivision w points
