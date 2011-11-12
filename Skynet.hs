module Main where

import Data.Maybe (mapMaybe)
import Control.Monad.Reader.Class

import Ants
import BotMonad
import GameRunner
import Diffusion
import Point
import Util

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

doTurn :: GameParams -> BotMonad [Order]
doTurn _ = do
  gs <- ask
  let grid = diffuse (impute (world gs)) 20
      orders = mapMaybe (generateOrders grid) $ myAnts $ ants gs in
    return $ output (showGrid grid) orders


-- | This runs the game
main :: IO ()
main = (game doTurn)
