module Combat (findCombatants) where

import Ants

import GameParams()
import Point 

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

data AntSearch = AntSearch
  (Map Int [Ant])
  (Map Int [Ant])
  BoundingBox 

makeAntSearch :: GameParams -> [Ant] -> AntSearch
makeAntSearch gp ats = let
  bb = (rows gp, cols gp)
  r = M.fromListWith (++) $ map (makeRows bb) ats
  c = M.fromListWith (++) $ map (makeCols bb) ats in
  AntSearch r c bb
  where makeRows bb a = ((row bb $ (dumbPoint.pointAnt) a), [a])
        makeCols bb a = ((col bb $ (dumbPoint.pointAnt) a), [a])

findAntsNear :: AntSearch -> Int -> Ant -> [Ant]
findAntsNear (AntSearch antRows antCols box@(r,c)) d a = let
  ap = (dumbPoint.pointAnt) a
  ar = row box ap
  ac = col box ap
  rowSet = toSet $ sliceRow antRows (ar - d) (ar + d)
  colSet = toSet $ sliceCol antCols (ac - d) (ac + d) in
  S.toList $ S.union rowSet colSet
  where slice mx mp low high =
          if low >= 0 && high < mx
          then fst $ M.split (high + 1) (snd (M.split low mp))
          else if low < 0 
               then M.intersectionWith (++) 
                    (snd $ M.split (mx + low - 1) mp) 
                    (fst $ M.split (high + 1) mp)
                else M.intersectionWith (++)
                     (snd $ M.split (low - 1) mp)
                     (fst $ M.split (high - mx + 1) mp)
        sliceRow = slice r
        sliceCol = slice c
        toSet mp = S.fromList $ concat $ M.elems mp

findCombatants :: GameParams -> [Ant] -> Map Ant [Ant]
findCombatants gp al =
    let as = makeAntSearch gp al in
      M.fromList $
      filter ((any isMe).snd) $ 
      filter ((any isEnemy).snd) $ 
      zip al $ map (\ a -> findAntsNear as 3 a) al
