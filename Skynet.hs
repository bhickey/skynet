module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.IO
import System.Random

import Ants
import Search
import BotMonad

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

generateOrders :: PathFinder -> [Point] -> Ant -> Maybe Order
generateOrders s targets a =
  let ap = pointAnt a
      pth = minimumBy (\ a b -> compare (len a) (len b)) $ map (s ap) targets in
    if (length $ path pth) > 0
    then Just $ Order a (head $ path pth)
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
  -- generate orders for all ants belonging to me
  let generatedOrders = map generateOrders $ myAnts $ ants gs
  -- for each ant take the first "passable" order, if one exists
      orders = mapMaybe (tryOrder (world gs)) generatedOrders
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining 
  lift . hPutStrLn stderr . show  $ elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = (game doTurn)
