module Order(
    Order (..)
  , FinalOrder (..)
  , RankedOrders (..)
  , finalizeOrders,
)where              

import Prelude hiding (null)
import Data.Map (Map)
import qualified Data.Map as M

import Point hiding (Point)
import Ants



data Order = Stay | Move !Direction 
data FinalOrder = FinalOrder !Ant !Order
--Stay is always the least desired order because it can always be fufilled
--Thus it is represented by the end of the list
data RankedOrders = RankedOrders !Ant [Direction]

type Occupied = Map SmartPoint RankedOrders

finalizeOrders :: [RankedOrders] -> [FinalOrder]
finalizeOrders orders = makeOrders orders M.empty

makeOrders :: [RankedOrders] -> Occupied -> [FinalOrder]
makeOrders (order@(RankedOrders ant []):rest) occ =
  case M.lookup p occ of
   Just evicted -> makeOrders (removeOrder evicted:rest) nextOcc 
   Nothing -> makeOrders rest nextOcc
 where
  p = (pointAnt ant)
  nextOcc = (M.insert p order occ)
  removeOrder (RankedOrders a (_:tl)) = (RankedOrders a tl)
  removeOrder (RankedOrders _ []) = error "Evicted from own square"

makeOrders (order@(RankedOrders ant (dir:tl)):rest) occ =
  case M.lookup p occ of
   Just _ -> makeOrders ((RankedOrders ant tl):rest) occ
   Nothing -> makeOrders rest (M.insert p order occ)
 where
  p = neighbor (pointAnt ant) dir

makeOrders [] occ = map construct . M.elems $ occ
 where construct (RankedOrders ant []) = FinalOrder ant Stay
       construct (RankedOrders ant (dir:_)) = FinalOrder ant (Move dir)

