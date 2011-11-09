module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad.Reader.Class
import Control.Monad.Random
import System.IO

import Ants
import BotMonad
import GameRunner

import Diffusion

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

generateOrders :: World -> DiffusionGrid -> Ant -> Maybe Order
generateOrders w d a@(Ant p _) = tryOrder w $ map (\ d -> Order a d) (bestScore d p)

{- |
 - Implement this function to create orders.
 - It uses an Monad with IO so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}

doTurn :: GameParams -> BotMonad [Order]
doTurn gp = do
  gs <- ask
  let grid = (iterate (diffuse rule) $ diffusionGrid $ impute (world gs)) !! 15
      orders = mapMaybe (generateOrders (world gs) grid) $ myAnts $ ants gs in
    return orders


-- | This runs the game
main :: IO ()
main = (game doTurn)
