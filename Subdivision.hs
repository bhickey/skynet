module Subdivision where

import Prelude hiding (null)

import Ants
import GameParams
import Point
import Data.Queue

import Data.Set (Set)
import qualified Data.Set as S

controlPoints :: GameParams -> World -> Queue (Int, SmartPoint)
controlPoints gp w =
  let rs = fsqrt $ rows gp
      cs = fsqrt $ cols gp in
    fromList $ map f [(r,c) | r <- [0,rs..rows gp], c <- [0,cs..cols gp]]
    where fsqrt = floor.sqrt.fromIntegral
          f = undefined

search :: Queue (Int, SmartPoint) -> Set SmartPoint -> [(Int, SmartPoint)]
search queue closed =
  if null queue
  then []
  else let (divId, sp) = peek queue
           queue' = dequeue queue
           n = foldl (\ a x -> if (S.member x closed) then a else x:a) [] (neighbors sp) in
         n ++ (search queue' closed)
