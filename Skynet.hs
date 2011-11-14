module Main where

import Data.Maybe (mapMaybe)
import Control.Monad.Reader.Class

import Ants
import BotMonad
import GameRunner
import Diffusion
import Control.Parallel.Strategies
--import Util
import Logging


-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder _ [] = Nothing
tryOrder _ o = Just $ head o

generateOrders :: DiffusionGrid -> Ant -> Maybe Order
generateOrders d a@(Ant p _) = Just $ Order a (diffusionScore d p)

{- |
 - Implement this function to create orders.
 - It uses an Monad with IO so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}

doTurn :: Logger -> GameParams -> BotMonad [Order]
doTurn logger gp = do
  logString logger "Start Turn"
  gs <- ask
  let grid = diffuse (smartVector gp) (impute (world gs)) 40
      orders = withStrategy (evalList rseq) $ mapMaybe (generateOrders grid) $ myAnts $ ants gs in
    do logString logger ('\n':(showGrid (rows gp,cols gp) grid))
       seq orders $ logString logger "End Turn"
       return orders


-- | This runs the game
main :: IO ()
main =
 do
  logs <- makeLogDirectory 
  logger <- makeLogger logs "turn-times"
  (game (doTurn logger))
