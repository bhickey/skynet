module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad.Reader.Class
import Control.Monad.Random
import System.IO

import Ants
import BotMonad
import GameRunner

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

generateOrders :: PathFinder -> [Point] -> Ant -> Maybe Order
generateOrders s targets t =
  let ap = pointAnt t
      pth = minimumBy (\ a b -> compare (len a) (len b)) $ map (s ap) targets in
    if (length $ path pth) > 0
    then Just $ Order t (head $ path pth)
    else Nothing

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
  let targets = (rc,rr):(food gs)
      searchFn = search gp (world gs)
      orders = mapMaybe (generateOrders searchFn targets) $ myAnts $ ants gs in
    debugMessage orders >>
    return orders


-- | This runs the game
main :: IO ()
main = (game doTurn)
