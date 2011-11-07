module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Random
import System.IO
import System.Random

import Ants
import BotMonad

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

generateOrders :: Ant -> Maybe Order
generateOrders a = Nothing

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
  rc <- getRandomR (0, cols gp)
  rr <- getRandomR (0, rows gp)
  let orders = mapMaybe generateOrders $ myAnts $ ants gs in
    lift $ hPutStrLn stderr (show orders) >>
    return orders


-- | This runs the game
main :: IO ()
main = (game doTurn)
