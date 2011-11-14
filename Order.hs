module Order where              

import Prelude hiding (null)
import Data.Queue
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Point
import Ants

type AntQueue = Queue Ant
type Orders = Map Ant [Point] 
type Occupied = Map Point Ant

orderAll :: Orders -> [Order]
orderAll od =
  let queue = fromList $ M.keys od in
    makeOrders queue od M.empty

makeOrders :: AntQueue -> Orders -> Occupied -> [Order]
makeOrders aq od occ =
  if null aq
  then []
  else let a = peek aq
           aq' = dequeue aq
           myOrders = fromJust $ M.lookup a od in
         case myOrders of
           [] -> maybeEvict a aq' (M.delete a od) occ
           hd:tl -> noEvict a hd aq' (M.insert a tl od) occ

maybeEvict :: Ant -> AntQueue -> Orders -> Occupied -> [Order]
maybeEvict a aq od occ =
  let aq' = if M.member (pointAnt a) occ
            then enqueue aq (fromJust $ M.lookup (pointAnt a) occ)
            else aq in
    makeOrders aq od (M.insert (pointAnt a) a occ)

noEvict :: Ant -> Point -> AntQueue -> Orders -> Occupied -> [Order]
noEvict a p aq od occ =
  if M.member (pointAnt a) occ
  then makeOrders (enqueue aq a) od occ
  else (makeOrder a p):(makeOrders aq od (M.insert p a occ))

makeOrder :: Ant -> Point -> Order
makeOrder a p = undefined
