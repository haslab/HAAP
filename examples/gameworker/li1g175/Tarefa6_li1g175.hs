module Tarefa6_li1g175  where

import Bomberman
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import qualified Data.Set as Set

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = decide (parse mapa) player ticks

decide :: Mapa -> Int -> Int -> Maybe Char
decide m i tk = let ply = (players m!!i) in
                    case ply of
                        Nothing -> Nothing
                        Just ply' ->
                            let fl = sort $ floodBF tk i m [] [((psy ply'),[])] in
                                case fl of 
                                    [] -> Just 'B'
                                    (h:_) -> zoneToAction h

{-| a zone represents a reachable position and associated stats |-}
data Zone = Z { live :: Bool,
                psz :: (Int,Int), 
                path :: [Int], 
                danger :: [Int], 
                powrs :: Bool, 
                boxes :: Int, 
                plys :: Int, 
                dist :: Float }
  deriving (Show, Eq)
      
instance Ord (Zone) where
    z1 <= z2 = rank z1 <= rank z2

rank :: Zone -> (Int,Float,Int,Int,Float,Int,Int,Float,Int)
rank z = (lv,ds,pw,pl,di,dsw,plw,di,lp)
  where lp = length (path z)
        ds = foldr (\a b -> (fromIntegral a) + (0.9 * b)) 0 (danger z)
        dsw = lp
        pl = - plys z
        plw = if (pl == 0) then 0 else lp
        pw = fromEnum (powrs z && lp <= 1)
        di = dist z
        lv = fromEnum $ not (live z)

{-| decides how to act given the target zone (moves unless already there, 
  in which case bombs if something to destroy) |-}
zoneToAction :: Zone -> Maybe Char
zoneToAction (Z _ _ [] _ _ 0 0 _) = Nothing
zoneToAction (Z _ _ [] _ _ y b _) = Just 'B' 
zoneToAction (Z _ _ l _ _ y b _) = case last l of
    0 -> Just $ dirToDir 0
    1 -> Just $ dirToDir 1
    2 -> Just $ dirToDir 2  
    3 -> Just $ dirToDir 3
    _ -> Nothing
 
floodBF :: Int -> Int -> Mapa -> [(Int,Int)] -> [((Int,Int),[Int])] -> [Zone]
floodBF _ _ _ _ [] = []
floodBF tk i m vs qs = map (zonify tk m i) qs ++ floodBF tk i m vs' (candidatesTop tk m vs' qs)
  where vs' = vs ++ map fst qs

zonify :: Int -> Mapa -> Int -> ((Int,Int),[Int]) -> Zone
zonify tk m i (pos,pth) = Z liv pos pth dangers sur_pw sur_bx sur_pl (distance pos (center m))
    where dangers = [fromEnum (pos `elem` explosionTime m i) + fromEnum (Just pos == espirala0 tk (length (grid m)) i) | i <- [1..6]]
          sur_pl = (length $ getSurPlayerExcept i m pos)
          sur_pw = (isJust (getPower m pos))
          sur_bx = (length $ getSuroundingBoxes m pos)
          liv = safe tk m pos (if pth == [] then 1 else length pth)

candidatesTop :: Int -> Mapa -> [(Int,Int)] -> [((Int,Int),[Int])] -> [((Int,Int),[Int])]
candidatesTop _ _ _ [] = []
candidatesTop tk m vs (p:t) = let cs = candidates tk m vs p in
                              cs ++ candidatesTop tk m (map fst cs ++ vs) t

candidates :: Int -> Mapa -> [(Int,Int)] -> ((Int,Int),[Int]) -> [((Int,Int),[Int])]
candidates tk m vs (pos,pth) | length pth >= 10 = []
                             | otherwise = filter vld [(stepDir pos i, i:pth) | i <- [0..3]]
  where vld (p,h) = canMove m p && safe tk m p (length h) && not (p `elem` vs)


