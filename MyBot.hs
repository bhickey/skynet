module Main where

import Control.Monad.Reader.Class

import Ants
import BotMonad
import GameRunner
import Point
import Order
import Searches
import Control.Parallel.Strategies
--import Util
import Logging                    
import Neighbors
import Data.List (sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Vector (Vector, (!))
                               
generateOrder :: Vector (Maybe (Food, Ant, Int, Direction)) -- food
              -> Vector (Maybe (Ant, Int, Direction))       -- enemy ants
              -> Vector (Maybe (Hill, Int, Direction))      -- enemy hills
              -> Vector (Maybe (SmartPoint, Int, Direction)) -- unseen
              -> Vector (Maybe (Int, Direction))             -- unknown
              -> Vector (Maybe (Ant, Int, Direction))        -- hill unseen
              -> Ant 
              -> RankedOrders 
generateOrder fd enmy hll uns unk huns a = 
  let ap = dumbPoint $ pointAnt a
      hillMove = case hll ! ap of
                    Just (_, dst, dir) -> Just (dst, dir)
                    Nothing -> Nothing
      enemyMove = case enmy ! ap of
                    Just (_, dst, dir) -> Just (if dst > 4 then dst else dst + 8, dir)
                    Nothing -> Nothing
      foodMove = case fd ! ap of
                   Just (_, a', dst, dir) -> 
                     if a == a'
                     then Just (max (dst - 2) 0, dir)
                     else if isEnemy a'
                          then Just (dst, dir)
                          else Nothing
                   Nothing -> Nothing
      myHillMove = case huns ! ap of
                     Nothing -> Nothing
                     Just (a', dst, dir) ->
                      if a == a'
                      then case enemyMove of
                        Nothing -> Nothing
                        Just (eDst, _) -> Just (max dst (eDst - 1), dir)
                      else Nothing
      unseenMove = case uns ! ap of
                     Nothing -> Nothing
                     Just (_, dst, dir) -> Just (4 * dst + 15, dir)
      unknownMove = case unk ! ap of
                     Nothing -> Nothing
                     Just (dst, dir) -> Just (dst, dir) in
    (integrateMoves a $ catMaybes $ [enemyMove, foodMove, unseenMove, hillMove, unknownMove, myHillMove])

integrateMoves :: Ant -> [(Int, Direction)] -> RankedOrders
integrateMoves a mvs =
    RankedOrders a $
    map fst $ sortBy compareSnd $
    neighborList $ foldl 
    (\ (Neighbors n e s w) (d, dir) ->
      case dir of 
        North -> Neighbors (n + d) e s w 
        East  -> Neighbors n (e + d) s w
        South -> Neighbors n e (s + d) w
        West  -> Neighbors n e s (w + d)) 
    (Neighbors 0.0 0.0 0.0 0.0)
    (map (\ (dist, dir) -> ((invSqrt dist), dir)) mvs)
    where compareSnd (_,s) (_,t) = compare t s
          invSqrt x = (1.0::Float) / (fromIntegral x)

{- |
 - Implement this function to create orders.
 - It uses an Monad with IO so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}

doTurn :: Logger -> GameParams -> BotMonad [FinalOrder]
doTurn logger gp = do
  logString logger "Start Turn"
  gs <- ask
  let owner = ownership gs
      nFood = nearestFood gs owner
      unseen = nearestUnseen gp gs
      hillUnseen = toUnseen gs owner unseen (myHills $ hills gs)
      unknown = nearestUnknown gp gs 
      enemy = nearestEnemy gs
      badHills = nearestHill gs
      orders = withStrategy (evalList rseq) . finalizeOrders . map (generateOrder nFood enemy badHills unseen unknown hillUnseen) . myAnts $ ants gs in
    do --logString logger ('\n':(showGrid (rows gp,cols gp) grid))
       --seq orders $ logString logger "End Turn"
       return orders

toUnseen :: GameState 
         -> Vector Ant
         -> Vector (Maybe (SmartPoint, Int, Direction))
         -> [Hill]
         -> Vector (Maybe (Ant, Int, Direction))
toUnseen gs av un hl =
  toPoints gs (map (\ (x,_,_) -> (x, av ! (dumbPoint x))) $ mapMaybe (\ h -> un ! (dumbPoint (pointHill h))) hl)


-- | This runs the game
main :: IO ()
main =
 do
  logs <- makeLogDirectory 
  logger <- makeLogger logs "turn-times"
  (game (doTurn logger))
