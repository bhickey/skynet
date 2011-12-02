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
import Data.Vector (Vector, (!))
                               
generateOrder :: Vector (Maybe Direction)
              -> Ant 
              -> RankedOrders 
generateOrder un a = 
  let ap = dumbPoint $ pointAnt a
      maybeDir = un ! ap in
    case maybeDir of
      Nothing -> RankedOrders a []
      Just dir -> RankedOrders a [dir]

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
  let unseen = nearestUnseen gp gs 
      orders = withStrategy (evalList rseq) . finalizeOrders . map (generateOrder unseen) . myAnts $ ants gs in
    do --logString logger ('\n':(showGrid (rows gp,cols gp) grid))
       --seq orders $ logString logger "End Turn"
       return orders


-- | This runs the game
main :: IO ()
main =
 do
  logs <- makeLogDirectory 
  logger <- makeLogger logs "turn-times"
  (game (doTurn logger))
