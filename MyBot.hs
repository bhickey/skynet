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
                               
generateOrder :: Vector (Food, Ant, Direction)
              -> Vector Direction
              -> Ant 
              -> RankedOrders 
generateOrder fd un a = 
  let ap = dumbPoint $ pointAnt a
      (_,owner,foodDir) = fd ! ap
      unseenDir = un ! ap in
    if owner == a
    then RankedOrders a [foodDir, unseenDir]
    else RankedOrders a [unseenDir, foodDir]

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
      foodOwner = nearestFood gs owner
      unseen = nearestUnseen gp gs 
  let orders = withStrategy (evalList rseq) . finalizeOrders . map (generateOrder foodOwner unseen) . myAnts $ ants gs in
    do --logString logger ('\n':(showGrid (rows gp,cols gp) grid))
       seq orders $ logString logger "End Turn"
       return orders


-- | This runs the game
main :: IO ()
main =
 do
  logs <- makeLogDirectory 
  logger <- makeLogger logs "turn-times"
  (game (doTurn logger))
