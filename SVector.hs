module SVector where

import Point (SmartPoint, dumbPoint)

import Data.Vector (Vector)
import qualified Data.Vector as V

data SVector a = SVector { vec :: Vector a }

(!) :: SVector a -> SmartPoint -> a
(!) sv sp = (V.!) (vec sv) (dumbPoint sp)
