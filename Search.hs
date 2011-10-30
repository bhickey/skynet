module Search where

import Prelude hiding (null)
import Ants
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Maybe

type Heuristic = (Point -> Int)

data Search = Search
  { queue :: I.IntMap [Point]
  , heuristic :: Heuristic }

data Path = Path
  { path :: [Direction]
  , len :: Int }

type PathFinder = (Point -> Point -> Path)
type SearchNode = (Point, Maybe Point)
type ClosedList  = M.Map Point (Maybe Point)
type NeighborService = (Point -> [SearchNode])

push :: Search -> Point -> Search
push sq p = Search 
  { queue = I.insertWith (++) ((heuristic sq) p) [p] (queue sq)
  , heuristic = heuristic sq }

pop :: Search -> (Point, Search)
pop sq = 
  case I.findMin $ queue sq of
    (k, [h]) -> (h, Search { queue = I.delete k $ queue sq, heuristic = heuristic sq})
    (k, h:t) -> (h, Search { queue = I.insert k t $ queue sq, heuristic = heuristic sq})

empty :: GameParams -> Point -> Search
empty wp p = Search 
  { queue = I.empty
  , heuristic = distance wp p }

null :: Search -> Bool
null = I.null . queue

enqueueNode :: (Search, ClosedList) -> SearchNode -> (Search, ClosedList)
enqueueNode acc@(sq, cl) (p, d) =
  if M.member p cl
  then acc
  else ((push sq p), (M.insert p d cl))

finished :: Search -> Bool
finished s = null s || 0 == (fst . I.findMin $ (queue s))

search :: GameParams -> World -> Point -> Point -> Path
search gp w start end =
  let pth = backtrace end $ astar (neighbors gp w)
                                  (push (empty gp end) start)
                                  (M.singleton start Nothing) in
    Path pth (length pth)

step :: Point -> Point -> Direction
step (x0,y0) (x1, y1) = 
  let xd = x1 - x0
      yd = y1 - y0 in
    dir xd yd where
    dir (-1) 0  = East
    dir  1 0  = West
    dir  0 (-1) = South
    dir  0 1  = North
    dir xd 0 = if xd > 0 then East else West
    dir 0 yd = if yd > 0 then South else North

backtrace :: Point -> ClosedList -> [Direction]
backtrace p cl = 
  case M.lookup p cl of
    Just p' -> 
        case p' of
          Nothing -> []
          Just p'' -> (step p p''):(backtrace p'' cl)
    Nothing -> []
  
astar :: NeighborService -> Search -> ClosedList -> ClosedList
astar ns s cl =
  if finished s
  then cl
  else case pop s of
         (p, sq) -> let (s',cl') = foldl enqueueNode (sq, cl) (ns p) in
                      astar ns s' cl'
