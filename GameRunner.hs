module GameRunner where

import Control.Monad.Random
import Ants
import BotMonad
import Data.Time
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.List (isPrefixOf, foldl')
import Data.Char (digitToInt)
import Data.Maybe

import System.IO

import Util
import Point






--------------------------------------------------------------------------------
-- Updating Game ---------------------------------------------------------------
--------------------------------------------------------------------------------
type MWorld s = STArray s Point MetaTile


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
           -> Point -- center point
           -> World
addVisible w gp p = 
  runSTArray $ do 
    w' <- unsafeThaw w
    mapM_ (setVisible w') (viewCircle gp p)
    return w'

updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s
  | "f" `isPrefixOf` s = -- add food
      let p = toPoint.tail $ s
          fs' = p:food gs
          nw = writeTile (world gs) p (LandTile FoodItem)
      in GameState nw (ants gs) fs' (hills gs) (startTime gs)
  | "w" `isPrefixOf` s = -- add water
      let p = toPoint.tail $ s
          nw = writeTile (world gs) p WaterTile
      in GameState nw (ants gs) (food gs) (hills gs) (startTime gs)
  | "h" `isPrefixOf` s = -- add hill
      let p = toPoint.init.tail $ s
          own = toOwner.digitToInt.last $ s
          hs = Hill { pointHill = p, ownerHill = own}:hills gs
          nw = writeTile (world gs) p . LandTile . HillItem $ own
      in GameState nw (ants gs) (food gs) hs (startTime gs)
  | "a" `isPrefixOf` s = -- add ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          as' = Ant { pointAnt = p, ownerAnt = own}:ants gs
          nw = writeTile (world gs) p . LandTile . LiveAntItem $ own
          nw' = if own == Me then addVisible nw gp p else nw
      in GameState nw' as' (food gs) (hills gs) (startTime gs)
  | "d" `isPrefixOf` s = -- add dead ant
      let own = toOwner.digitToInt.last $ s
          p = toPoint.init.tail $ s
          nw = writeTile (world gs) p . LandTile . DeadAntItem $  own
      in GameState nw (ants gs) (food gs) (hills gs) (startTime gs)
  | otherwise = gs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = (uncurry $ point gp) .tuplify2.map read.words
    writeTile w p t = runSTArray $ do
      w' <- unsafeThaw w
      writeArray w' p MetaTile {tile = t, visible = Observed}
      return w'

initialWorld :: GameParams -> World
initialWorld gp = 
  let r = rows gp
      c = cols gp in
    listArray ((point gp 0 0), (point gp (r - 1) (c - 1))) . repeat $ MetaTile UnknownTile Unobserved

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      r  = lookup' "rows"
      c  = lookup' "cols"
  in GameParams { loadtime      = lookup' "loadtime"
                , turntime      = lookup' "turntime"
                , rows          = r
                , cols          = c
                , turns         = lookup' "turns"
                , playerSeed    = lookup' "player_seed"
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                --, viewCircle    = vp
                --, attackCircle  = ap
                --, spawnCircle   = sp
                }

modifyWorld :: MWorld s -> (MetaTile -> MetaTile) -> Point -> ST s ()
modifyWorld mw f p = do
  e' <- readArray mw p
  e' `seq` writeArray mw p (f e') -- !IMPORTANT! seq is necessary to avoid space leaks

mapWorld :: (MetaTile -> MetaTile) -> World -> World
mapWorld f w = runSTArray $ do
  mw <- unsafeThaw w
  mapM_ (modifyWorld mw f) (indices w)
  return mw




gameLoop :: GameParams 
         -> BotMonad [Order]
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
      mapM_ issueOrder orders
      finishTurn
      gameLoop gp doTurn (mapWorld clearMetaTile $ world gs) (tail $ snd cs) -- clear world for next turn
  | "end" `isPrefixOf` line = endGame input
  | otherwise = gameLoop gp doTurn w input
gameLoop _ _ _ [] = endGame []

game :: (GameParams -> BotMonad [Order]) -> IO ()
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

issueOrder :: Order -> IO ()
issueOrder (Order ant direction) = do
  let srow = (show . row . pointAnt) ant
      scol = (show . col . pointAnt) ant 
      sdir = show direction
  putStrLn $ "o " ++ srow ++ " " ++ scol ++ " " ++ sdir




