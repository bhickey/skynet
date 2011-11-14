module GameRunner where

import Control.Monad.Random
import Ants
import BotMonad
import Data.Time
import Control.Monad.ST

import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import System.IO

import Util
import Point
import Order
import GameParams






--------------------------------------------------------------------------------
-- Updating Game ---------------------------------------------------------------
--------------------------------------------------------------------------------
type MWorld s = MV.MVector s MetaTile


-- | Resets tile to land if it is currently occupied by food or ant
--   and makes the tile invisible.
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m
  | fOr (tile m) [isLiveAnt, isFood, isDeadAnt] = MetaTile (LandTile BlankItem) Unobserved
  | otherwise = MetaTile (tile m) Unobserved

setVisible :: MWorld s -> Point -> ST s ()
setVisible mw p = do
  modifyWorld mw visibleMetaTile p

addVisible :: World
           -> GameParams -- viewPoints
           -> SmartPoint -- center point
           -> World
addVisible w gp p = 
  V.create $ do 
    --w' <- V.unsafeThaw w
    w' <- V.thaw w
    mapM_ (setVisible w' . dumbPoint) (viewCircle gp p)
    return w'

updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s
  | "f" `isPrefixOf` s = -- add food
      let p = toPoint.tail $ s
          fs' = p:food gs
          nw = writeTile (world gs) (dumbPoint p) (LandTile FoodItem)
      in GameState nw (ants gs) fs' (hills gs) (startTime gs)
  | "w" `isPrefixOf` s = -- add water
      let p = toPoint.tail $ s
          nw = writeTile (world gs) (dumbPoint p) WaterTile
      in GameState nw (ants gs) (food gs) (hills gs) (startTime gs)
  | "h" `isPrefixOf` s = -- add hill
      let p = toPoint.init.tail $ s
          own = toOwner.digitToInt.last $ s
          hs = Hill { pointHill = p, ownerHill = own}:hills gs
          nw = writeTile (world gs) (dumbPoint p) . LandTile . HillItem $ own
      in GameState nw (ants gs) (food gs) hs (startTime gs)
  | "a" `isPrefixOf` s = -- add ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          as' = Ant { pointAnt = p, ownerAnt = own}:ants gs
          nw = writeTile (world gs) (dumbPoint p) . LandTile . LiveAntItem $ own
          nw' = if own == Me then addVisible nw gp p else nw
      in GameState nw' as' (food gs) (hills gs) (startTime gs)
  | "d" `isPrefixOf` s = -- add dead ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          nw = writeTile (world gs) (dumbPoint p) . LandTile . DeadAntItem $  own
      in GameState nw (ants gs) (food gs) (hills gs) (startTime gs)
  | otherwise = gs -- ignore line
  where
    toPoint :: String -> SmartPoint
    toPoint = (uncurry $ smartGrid gp) .tuplify2.map read.words
    writeTile w p t = V.create $ do
      --w' <- V.unsafeThaw w
      w' <- V.thaw w
      MV.write w' p MetaTile {tile = t, visible = Observed}
      return w'

initialWorld :: GameParams -> World
initialWorld gp = 
  let r = rows gp
      c = cols gp in
      V.replicate (r*c) $ MetaTile UnknownTile Unobserved

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      r  = lookup' "rows"
      c  = lookup' "cols"
      (grid,vector) = smartWorld (r,c)
  in GameParams { loadtime      = lookup' "loadtime"
                , turntime      = lookup' "turntime"
                , rows          = r
                , cols          = c
                , turns         = lookup' "turns"
                , playerSeed    = lookup' "player_seed"
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , smartGrid = grid
                , smartVector = vector
                }

modifyWorld :: MWorld s -> (MetaTile -> MetaTile) -> Point -> ST s ()
modifyWorld mw f p = do
  e' <- MV.read mw p
  e' `seq` MV.write mw p (f e') -- !IMPORTANT! seq is necessary to avoid space leaks




gameLoop :: GameParams 
         -> BotMonad [FinalOrder]
         -> World
         -> [String] -- input
         -> IO ()
gameLoop gp doTurn w (line:input)
  | "turn" `isPrefixOf` line = do
      hPutStrLn stderr line
      time <- getCurrentTime
      let cs = break (isPrefixOf "go") input
          gs = foldl' (updateGameState gp) (GameState w [] [] [] time) (fst cs)
      gen <- newStdGen
      orders <- runBotMonad doTurn gs gen
      mapM_ (issueOrder gp) orders
      finishTurn
      gameLoop gp doTurn (V.map clearMetaTile $ world gs) (tail $ snd cs) -- clear world for next turn
  | "end" `isPrefixOf` line = endGame input
  | otherwise = gameLoop gp doTurn w input
gameLoop _ _ _ [] = endGame []

game :: (GameParams -> BotMonad [FinalOrder]) -> IO ()
game doTurn = do
  content <- getContents
  let cs = break (isPrefixOf "ready") $ lines content
      gp = createParams $ map (tuplify2.words) (fst cs)
  finishTurn
  gameLoop gp (doTurn gp) (initialWorld gp) (tail $ snd cs)



-- TODO this could be better
endGame :: [String] -> IO ()
endGame input = do
  hPutStrLn stderr "end of game"
  mapM_ (hPutStrLn stderr) input


-- | Tell engine that we have finished the turn or setting up.
finishTurn :: IO ()
finishTurn = do
  putStrLn "go" 
  hFlush stdout

issueOrder :: GameParams -> FinalOrder -> IO ()
issueOrder _ (FinalOrder _ Stay) = return ()
issueOrder gp (FinalOrder ant (Move direction)) = do
  let srow = (show . row box . dumbPoint . pointAnt) ant
      scol = (show . col box . dumbPoint . pointAnt) ant 
      sdir = show direction
  putStrLn $ "o " ++ srow ++ " " ++ scol ++ " " ++ sdir
  where box = (rows gp,cols gp)




