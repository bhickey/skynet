module Subdivision where

import Ants

import Data.Array
import Data.Maybe
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

subdivide :: World -> r -> (World -> Point -> [Point]) -> Subdivision
subdivide w rng nfn = emptySubdivision w
