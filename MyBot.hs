module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Ants
import BotMonad
import GameRunner
import Order
import Control.Parallel.Strategies
import System.Random
import Data.List (permutations)
--import Util
import Logging
                               
randDirections :: Int -> [Direction]
randDirections x = (permutations [North,South,East,West]) !! x

generateOrder :: Int -> Ant -> RankedOrders 
generateOrder x a = RankedOrders a (randDirections x)

{- |
 - Implement this function to create orders.
 - It uses an Monad with IO so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}

doTurn :: Logger -> GameParams -> BotMonad [FinalOrder]
doTurn logger _ = do
  logString logger "Start Turn"
  gs <- ask
  rnd <- liftIO $ randomRIO (0,23)
  let orders = withStrategy (evalList rseq) . finalizeOrders . map (generateOrder rnd) . myAnts $ ants gs in
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
